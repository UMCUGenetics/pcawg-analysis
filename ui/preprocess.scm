(define-module (ui preprocess)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 threads)
  #:use-module (logger)
  #:use-module (pcawg config)
  #:use-module (pcawg dcc-portal)
  #:use-module (pcawg filesystem)
  #:use-module (pcawg hartwig)
  #:use-module (srfi srfi-1)
  #:use-module (json)

  #:export (do-preprocess))

(define program-options
  '((access-token           (single-char #\a) (value #t))
    (archive-bucket         (single-char #\b) (value #t))
    (cache-directory        (single-char #\c) (value #t))
    (debug-log              (single-char #\d) (value #t))
    (error-log              (single-char #\e) (value #t))
    (gen3-metadata          (single-char #\G) (value #t))
    (google-cmek-path       (single-char #\k) (value #t))
    (google-service-account (single-char #\S) (value #t))
    (google-project         (single-char #\g) (value #t))
    (google-region          (single-char #\r) (value #t))
    (pipeline-jar           (single-char #\j) (value #t))
    (donor-id               (single-char #\D) (value #t))
    (project-code           (single-char #\p) (value #t))
    (report-bucket          (single-char #\B) (value #t))
    (simultaneous-donors    (single-char #\t) (value #t))
    (store-directory        (single-char #\s) (value #t))
    (help                   (single-char #\h) (value #f))))

(define (show-help)
  (for-each (lambda (line) (display line) (newline))
   '("Options for 'preprocess':"
     "  --access-token=ARG         -a  The ICGC access token."
     "  --archive-bucket=ARG       -b  The archive bucket to use."
     "  --cache-dir=ARG            -c  Cache directory for HTTP requests."
     "  --debug-log                -d  Where to write the debug log."
     "  --error-log                -e  Where to write the error log."
     "  --gen3-metadata=ARG        -G  GEN3 file metadata."
     "  --google-cmek-path=ARG     -k  Path to the customer-managed encryption key."
     "  --google-project=ARG       -g  The Google project ID."
     "  --google-region=ARG        -r  The storage region to use."
     "  --google-service-account=A -S  The service account to use."
     "  --pipeline-jar=ARG         -j  Location of the JAR file for pipeline5."
     "  --donor-id=ARG             -D  Which PCAWG donor to process."
     "  --project-code=ARG         -p  Which PCAWG project to process."
     "  --report-bucket=ARG        -B  The patient report bucket to use."
     "  --simultaneous-donors=ARG  -t  Number of donors to process in parallel."
     "  --store-directory=ARG      -s  Where to store the data."
     "  --help                     -h  Show this message."))
  (exit 0))

(define (process-donor donor-id metadata)
  (log-debug "acontrol" "Processing ~a" donor-id)
  (let ((files (files-for-donor donor-id
                 (with-gen3-object-ids metadata (gen3-metadata)))))
    (if (every (lambda (t) t)
               (n-par-map 2 bam->fastq files))
        (run-pipeline donor-id)
        (begin
          (log-error "acontrol" "Preprocessing files for ~s failed." donor-id)
          #f))))

(define (do-preprocess options)
  (let ((config (getopt-long options program-options)))

    (when (assoc-ref config 'help)    (show-help))
    (when (assoc-ref config 'access-token)
      (set-access-token! (assoc-ref config 'access-token)))

    (when (assoc-ref config 'cache-directory)
      (set-cache-directory! (assoc-ref config 'cache-directory)))

    (when (assoc-ref config 'gen3-metadata)
      (set-gen3-metadata! (catch #t
														(lambda _
															(vector->list
															 (call-with-input-file
																	 (assoc-ref config 'gen3-metadata)
																 json->scm)))
														(lambda (key . args)
															'()))))

    (when (assoc-ref config 'google-project)
      (set-google-project! (assoc-ref config 'google-project)))

    (when (assoc-ref config 'google-region)
      (set-google-region! (assoc-ref config 'google-region)))

    (when (assoc-ref config 'google-cmek-path)
      (set-google-cmek-path! (assoc-ref config 'google-cmek-path)))

    (when (assoc-ref config 'archive-bucket)
      (set-google-archive-bucket! (assoc-ref config 'archive-bucket)))

    (when (assoc-ref config 'report-bucket)
      (set-google-report-bucket! (assoc-ref config 'report-bucket)))

    (when (assoc-ref config 'google-service-account)
      (set-google-service-account! (assoc-ref config 'google-service-account)))

    (when (assoc-ref config 'donor-id)
      (set-donor-id! (assoc-ref config 'donor-id)))

    (when (assoc-ref config 'project-code)
      (set-project-code! (assoc-ref config 'project-code)))

    (when (assoc-ref config 'pipeline-jar)
      (set-pipeline-jar! (assoc-ref config 'pipeline-jar)))

    (when (assoc-ref config 'store-directory)
      (set-store-directory! (assoc-ref config 'store-directory)))

    (when (assoc-ref config 'simultaneous-donors)
      (set-simultaneous-donors! (string->number
                                 (assoc-ref config 'simultaneous-donors))))

    (when (assoc-ref config 'debug-log)
      (set-default-debug-port!
       (open-file (assoc-ref config 'debug-log) "a")))

    (when (assoc-ref config 'error-log)
      (set-default-error-port!
       (open-file (assoc-ref config 'error-log) "a")))

    ;; ---------------------------------------------------------------------------
    ;; ENVIRONMENT VARIABLES
    ;; ---------------------------------------------------------------------------
    ;;
    ;; The Score client reads the environment variables “STORAGE_PROFILE” and
    ;; “ACCESSTOKEN”, so we set those in the program's runtime environment.
    ;

    (setenv "STORAGE_PROFILE" (storage-profile))
    (setenv "ACCESSTOKEN"     (access-token))

    ;; ---------------------------------------------------------------------------
    ;; PROCESSING
    ;; ---------------------------------------------------------------------------
    ;;
    ;

    (unless (assoc-ref config 'google-cmek-path)
      (format #t "Please specify the --google-cmek-path.~%")
      (exit 1))

    (unless (assoc-ref config 'archive-bucket)
      (format #t "Please specify the --archive-bucket.~%")
      (exit 1))

    (unless (assoc-ref config 'report-bucket)
      (format #t "Please specify the --report-bucket.~%")
      (exit 1))

    (unless (assoc-ref config 'google-service-account)
      (format #t "Please specify the --google-service-account.~%")
      (exit 1))

    (unless (or (project-code) (donor-id))
      (format #t "Please specify which project or donor to process.~%")
      (exit 1))

    (unless (pipeline-jar)
      (format #t "Please specify the location of the pipeline JAR.~%")
      (exit 0))

    (log-debug "acontrol" "Started.")
    (log-error "acontrol" "Started.")

    (catch 'json-invalid
      (lambda _
        (cond
         ((project-code)
          (let* ((metadata   (metadata-for-project (project-code)))
                 (all-donors (donors-in-project metadata))
                 (donors     (delete #f (map (lambda (donor-id)
                                               (if (donor-is-processed? donor-id)
                                                   #f
                                                   donor-id))
                                             all-donors))))
            (format #t "There are ~a donors of ~a left to process in ~s"
                    (length donors) (length all-donors) (project-code))

            ;; Do Pre-processing for donors.
            (log-debug "acontrol"
                       "Processing ~a donors in parallel."
                       (simultaneous-donors))

            (n-par-for-each (simultaneous-donors)
                            (lambda (donor-id)
                              (process-donor donor-id metadata))
                            donors)))
         ((donor-id)
          (let ((metadata (metadata-for-donor (donor-id))))
            (process-donor (donor-id) metadata)))
         (else
          (log-debug "acontrol" "Nothing to do."))))
      (lambda (key . args)
        (cond
         ((project-code)
          (format #t "The Collaboratory does not have any samples for project ~s.~%"
                  (project-code)))
         ((donor-id)
          (format #t "The Collaboratory does not have any files for donor ~s.~%"
                  (donor-id)))
         (else
          (format #t "Error: ~a: ~s~%" key args)))))))
