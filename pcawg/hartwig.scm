;;; Copyright © 2020  Roel Janssen <roel@gnu.org>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (pcawg hartwig)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 threads)
  #:use-module (json)
  #:use-module (pcawg config)
  #:use-module (pcawg filesystem)
  #:use-module (pcawg tools)
  #:use-module (pcawg google)
  #:use-module (logger)

  #:export (make-google-bucket
            name-google-bucket
            panel-file
            run-pipeline))

(define %max-concurrent-lanes 24)

(define (name-google-bucket donor-full-name)
  (string-append "gs://run-" (string-downcase donor-full-name) "-umc1"))

(define (make-google-bucket donor-full-name)
  (let ((name (name-google-bucket donor-full-name)))
    (if (zero? (system (string-append
                        %gsutil " mb"
                        " -p " (google-project)
                        " -l " (google-region)
                        " -c STANDARD "
                        name)))
        name
        #f)))

(define (panel-file donor-name)
  (let* ((filename        (string-append (donor-directory donor-name) "/panel.json"))
         (tumor           (string-append donor-name "T"))
         (reference       (string-append donor-name "R"))
         (tumor-lanes     (lanes-for-sample tumor))
         (reference-lanes (lanes-for-sample reference)))
    (if (and tumor-lanes reference-lanes)
        (begin
          (unless (file-exists? filename)
            (call-with-output-file filename
              (lambda (port)
                (scm->json port
                 `((tumor     . ((type  . "TUMOR")
                                 (name  . ,tumor)
                                 (lanes . ,tumor-lanes)))
                   (reference . ((type  . "REFERENCE")
                                 (name  . ,reference)
                                 (lanes . ,reference-lanes))))))))
          filename)
        #f)))

(define (lanes-per-donor donor-name)
  (catch #t
    (lambda _
      (call-with-input-file (panel-file donor-name)
        (lambda (port)
          (let* ((data (json->scm port)))
            (+ (length (hash-ref (hash-ref data "tumor") "lanes"))
               (length (hash-ref (hash-ref data "reference") "lanes")))))))
    (lambda (key . args)
      #f)))

(define (run-pipeline donor-name)
  (log-debug "run-pipeline" "Running for ~s" donor-name)
  (let ((reference-bucket (name-google-bucket (string-append donor-name "R")))
        (tumor-bucket     (name-google-bucket (string-append donor-name "T")))
        (lanes            (lanes-per-donor donor-name)))
    (log-debug "run-pipeline" "Processing ~s lanes for ~s" lanes donor-name)
    (cond
     [(and (bucket-exists? reference-bucket)
           (bucket-exists? tumor-bucket)
           lanes
           (may-run-pipeline-run? (google-region) lanes %max-concurrent-lanes))
      (let* ((logfile (lambda (file)
                        (string-append
                         (donor-directory donor-name) "/" file)))
             (command (string-append
                       %java " -jar " (pipeline-jar)
                       " -profile development"
                       " -set_id " donor-name
                       " -run_id from-jar"
                       " -preemptible_vms true"
                       " -max_concurrent_lanes " %max-concurrent-lanes
                       " -sample_json " (panel-file donor-name)
                       " -cloud_sdk " (string-drop-right
                                       (dirname %gcloud) 4)
                       " > " (logfile "/pipeline5.log")
                       " 2> "(logfile "/pipeline5.errors")))
             (port (open-input-pipe command)))
        (log-debug "run-pipeline" "Command:  ~a~%" command)
        (zero? (status:exit-val (close-pipe port))))]
     [else
      (log-debug "run-pipeline" "Retrying the pipeline run in 2 minutes.")
      (sleep 120)
      (run-pipeline donor-name)])))
