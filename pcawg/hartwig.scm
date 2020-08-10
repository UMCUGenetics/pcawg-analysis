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
  ;; From the GCP documentation:
  ;; > There is a per-project rate limit to bucket creation and deletion of
  ;; > approximately 1 request every 2 seconds.
  ;;
  ;; So we need to delay each bucket creation by two seconds to avoid
  ;; exceeding this rate limit.
  (let ((name (name-google-bucket donor-full-name)))
    (if (zero? (system (string-append
                        %gsutil " mb"
                        " -p " (google-project)
                        " -l " (google-region)
                        " -c STANDARD "
                        name)))
        (begin (sleep 2) name)
        #f)))

(define (panel-file donor-name)
  (let ((filename (string-append (donor-directory donor-name) "/panel.json")))
    (if (file-exists? filename)
        filename
        (let* ((tumor           (string-append donor-name "T"))
               (reference       (string-append donor-name "R"))
               (tumor-lanes     (lanes-for-sample tumor))
               (reference-lanes (lanes-for-sample reference)))
          (log-debug "panel-file"
                     "tumor-lanes: ~a."
                     (list->vector tumor-lanes))
          (if (and tumor-lanes reference-lanes)
              (call-with-output-file filename
                (lambda (port)
                  (scm->json
                   `((tumor     (type  . "TUMOR")
                                (name  . ,tumor)
                                (lanes . ,(list->vector tumor-lanes)))
                     (reference (type  . "REFERENCE")
                                (name  . ,reference)
                                (lanes . ,(list->vector reference-lanes))))
                   port)
                  filename))
              #f)))))

(define (lanes-per-donor donor-name)
  (catch #t
    (lambda _
      (let ((panel (panel-file donor-name)))
        (if panel
            (call-with-input-file panel
              (lambda (port)
                (let* ((data (json->scm port)))
                  (+ (vector-length (assoc-ref (assoc-ref data "tumor") "lanes"))
                     (vector-length (assoc-ref (assoc-ref data "reference") "lanes"))))))
            (throw 'missing-panel-file))))
    (lambda (key . args)
      (log-debug "lanes-per-donor" "Thrown: ~a: ~s" key args)
      #f)))

(define (run-pipeline donor-name)
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
             (panel   (panel-file donor-name))
             (command (string-append
                       %java " -jar " (pipeline-jar)
                       " -profile development"
                       " -output_cram false"
                       " -set_id " donor-name
                       " -run_id from-jar"
                       " -preemptible_vms true"
                       " -max_concurrent_lanes " (number->string %max-concurrent-lanes)
                       " -sample_json " panel
                       " -cloud_sdk " (dirname %gcloud)
                       " -archive_bucket " (google-archive-bucket)
                       " -patient_report_bucket " (google-report-bucket)
                       " -region " (google-region)
                       " -project " (google-project)
                       " -cmek " (google-cmek-path)
                       " -service_account_email " (google-service-account)
                       " > " (logfile "/pipeline5.log")
                       " 2> "(logfile "/pipeline5.errors"))))
        (log-debug "run-pipeline" "Command:  ~a~%" command)
        (let* ((port   (open-input-pipe command))
               (status (zero? (status:exit-val (close-pipe port)))))
          (log-debug "run-pipeline" "Pipeline ~a for ~s."
                     (if status "completed" "failed")
                     donor-name)))]
     [else
      (log-debug "run-pipeline" "Retrying the pipeline for ~s run in 2 minutes." donor-name)
      (sleep 120)
      (run-pipeline donor-name)])))
