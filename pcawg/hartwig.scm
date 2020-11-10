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
            run-pipeline
            donor-is-processed?
            remove-fastq-buckets-for-donor))

(define %max-concurrent-lanes 24)

(define (name-google-bucket donor-full-name)
  (string-append "gs://run-" (string-downcase donor-full-name) "-umc1"))

(define (make-google-bucket donor-full-name)
  (let ((name (name-google-bucket donor-full-name)))
    (cond
     [(bucket-exists? name)
      #t]
     [(zero? (system (format #f "~a mb -p ~a -l ~a -c STANDARD ~a"
                             %gsutil (google-project) (google-region) name)))
      ;; From the GCP documentation:
      ;; > There is a per-project rate limit to bucket creation and deletion of
      ;; > approximately 1 request every 2 seconds.
      ;;
      ;; So we need to delay each bucket creation by two seconds to avoid
      ;; exceeding this rate limit.
      (begin (sleep 2) name)]
     [else #f])))

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
        (trun-bucket      (name-google-bucket (string-append donor-name "T-from-jar")))
        (rrun-bucket      (name-google-bucket (string-append donor-name "T-from-jar")))
        (output-bucket    (format #f "gs://~a/~aT-from-jar" (google-report-bucket) donor-name))
        (lanes            (lanes-per-donor donor-name)))
    (cond
     [(or (bucket-exists? trun-bucket)
          (bucket-exists? rrun-bucket))
      (log-debug "run-pipeline" "Already running pipeline for ~s." donor-name)
      #t]
     [(bucket-exists? output-bucket)
      (log-debug "run-pipeline" "Already completed pipeline for ~s." donor-name)
      #t]
     [(and (bucket-exists? reference-bucket)
           (bucket-exists? tumor-bucket)
           lanes)
           ;(may-run-pipeline-run? (google-region) lanes %max-concurrent-lanes))
      (let* ((logfile (lambda (file)
                        (string-append
                         (donor-directory donor-name) "/" file)))
             (panel   (panel-file donor-name)))
        (if panel
            (let ((command (format #f "~a -Xmx64m -jar ~a -profile development -output_cram false -set_id ~a  -run_id from-jar -preemptible_vms true -max_concurrent_lanes ~a -sample_json ~a -cloud_sdk ~a -archive_bucket ~a -patient_report_bucket ~a -region ~a -project ~a -cmek ~a -poll_interval 30 -service_account_email ~a > ~a 2> ~a"
                                   %java (pipeline-jar) donor-name %max-concurrent-lanes panel (dirname %gcloud) (google-archive-bucket) (google-report-bucket) (google-region) (google-project) (google-cmek-path) (google-service-account) (logfile "/pipeline5.log") (logfile "/pipeline5.errors"))))

              (log-debug "run-pipeline" "Command:  ~a" command)
              #t)
              ;; (let* ((port   (open-input-pipe command))
              ;;        (status (zero? (status:exit-val (close-pipe port)))))
              ;;   (log-debug "run-pipeline" "Pipeline ~a for ~s."
              ;;              (if status "completed" "failed")
              ;;              donor-name))
            (begin
              (log-debug "run-pipeline" "~a's panel is incomplete, skipping run."
                         donor-name)
              #f)))]
     [else
      (log-debug "run-pipeline" "Skipping pipeline run for ~s." donor-name)
      #f])))

(define (donor-is-processed? donor-name)
  (let ((port (open-input-pipe
               (format #f "~a ls gs://patient-report-bucket-umc/~a-from-jar/linx/run.log > /dev/null 2> /dev/null"
                       %gsutil donor-name))))
    (zero? (status:exit-val (close-pipe port)))))

(define (remove-fastq-buckets-for-donor donor-name)
  (if (donor-is-processed? donor-name)
      (let ((tumor     (string-append donor-name "t"))
            (reference (string-append donor-name "r")))
        (for-each system
                  (list (format #f "~a rm -rf ~a" %gsutil (name-google-bucket tumor))
                        (format #f "~a rm -rf ~a" %gsutil (name-google-bucket reference)))))
      #f))
