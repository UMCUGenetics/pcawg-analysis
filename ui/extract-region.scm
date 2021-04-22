(define-module (ui extract-region)
  #:use-module (ice-9 format)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 threads)
  #:use-module (logger)
  #:use-module (pcawg filesystem)
  #:use-module (pcawg tools)
  #:use-module (pcawg bam-regions)
  #:use-module (pcawg google)
  #:use-module (srfi srfi-1)

  #:export (do-extract-region))

(define program-options
  '((report-bucket          (single-char #\b) (value #t))
    (debug-log              (single-char #\d) (value #t))
    (error-log              (single-char #\e) (value #t))
    (simultaneous-donors    (single-char #\t) (value #t))
    (store-directory        (single-char #\s) (value #t))
    (donor-id               (single-char #\D) (value #t))
    (region                 (single-char #\r) (value #t))
    (help                   (single-char #\h) (value #f))))

(define (show-help)
  (for-each (lambda (line) (display line) (newline))
   '("Options for 'extract-region':"
     "  --report-bucket=ARG        -b  The patient report bucket to use."
     "  --debug-log                -d  Where to write the debug log."
     "  --error-log                -e  Where to write the error log."
     "  --simultaneous-donors=ARG  -t  Number of donors to process in parallel."
     "  --store-directory=ARG      -s  Where to store the data."
     "  --donor=ARG                -D  Which donor to process.  When none is"
     "                                 specified, all donors will be processed."
     "  --region=ARG               -r  The region to extract."
     "  --help                     -h  Show this message."))
  (exit 0))

(define (donor-is-processed? storedir donor-id region)
  (let* ((done-ref (format #f "~a/~aR_~a.done" storedir donor-id region))
	 (done-tum (format #f "~a/~aT_~a.done" storedir donor-id region)))
    (and (file-exists? done-ref)
	 (file-exists? done-tum))))

(define (donor->extract-region bucket store-directory donor-id region)

  (define (extract-region suffix)
    (let* ((full-id     (string-append donor-id suffix))
           (input-file  (format #f "~a/~a-from-jar/~a/aligner/~a.bam"
                                bucket donor-id full-id full-id))
           (output-file (format #f "~a/~a_~a.bam"
                                store-directory full-id region))
           (done-file   (format #f "~a/~a_~a.done"
                                store-directory full-id region)))
      (log-debug "donor->extract-region" "Reading file: ~s" input-file)
      (if (file-exists? done-file)
          #t
          (receive (success? message)
              (extract-reads-for-region input-file output-file "bam" region)
            (if (not success?)
                (begin
                  (log-error "donor->extract-region" "Error: ~s: ~s"
                             full-id message)
                  #f)
                (begin
                  (call-with-output-file done-file
                    (lambda (port) (format port "")))
                  (log-debug "donor->extract-region" "Finished: ~s" full-id)
                  #t))))))

  (extract-region "T")
  (extract-region "R"))

(define (donors-from-bucket bucket-uri)

  (define* (donors-from-port port #:optional (output '()))
    (let ((directory (read-line port))
          (prefix-length (+ (string-length bucket-uri) 1)))
      (if (eof-object? directory)
          (map (lambda (uri) (substring uri prefix-length)) output)
          (let* ((cut-at    (string-contains directory "-from-jar"))
                 (remains   (if cut-at  (substring directory 0 cut-at) #f))
                 (last-char (if remains (string-ref remains (- cut-at 1)) #f))
                 (donor     (if (or (eq? last-char #\T)
                                    (eq? last-char #\R))
                                #f
                                remains)))
            (if donor
                (donors-from-port port (cons donor output))
                (donors-from-port port output))))))

  (let* ((command (format #f "~a ls ~a" %gsutil bucket-uri))
         (port    (open-input-pipe command))
         (output  (donors-from-port port)))
    (close-port port)
    (sort output string<)))

(define* (process-jobs donors number-of-jobs report-bucket store-directory
                       region #:optional (threads '()))
  (cond
   ((or (>= (length threads) number-of-jobs)
        (null? donors))

    ;; Wait for the current jobs to finish.
    (while (any not (map thread-exited? threads))
      (sleep 1))

    ;; Process the remaining donors.
    (if (null? donors)
        #t
        (begin
          ;; Force a garbage collection round now.
          (gc)

          ;; Refresh the auth token.
          ;; It sets the environment variable GCS_OAUTH_TOKEN as side effect,
          ;; so we don't need to do anything else.
          (log-debug "process-jobs" "Token expires in ~a seconds."
                     (token-expires-in (get-token)))

          ;; Continue processing more donors.
          (process-jobs donors number-of-jobs report-bucket store-directory region '()))))

   ;; Spawn multiple jobs.
   (else
    (let ((donor (car donors)))
      (if (donor-is-processed? store-directory donor region)
	  (begin
	    (log-debug "process-jobs" "~a is already done." donor)
	    (process-jobs (cdr donors) number-of-jobs report-bucket
			  store-directory region threads))
	  (process-jobs (cdr donors)
			number-of-jobs
			report-bucket
			store-directory
			region
			(cons (call-with-new-thread
                               (lambda _
				 (donor->extract-region
				  report-bucket store-directory donor region)))
                              threads)))))))

(define (do-extract-region options)
  (let ((config (getopt-long options program-options)))

    (when (assoc-ref config 'help) (show-help))

    (when (assoc-ref config 'debug-log)
      (set-default-debug-port!
       (open-file (assoc-ref config 'debug-log) "a")))

    (when (assoc-ref config 'error-log)
      (set-default-error-port!
       (open-file (assoc-ref config 'error-log) "a")))

    (unless (assoc-ref config 'simultaneous-donors)
      (cons `(simultaneous-donors . "1") config))

    (unless (assoc-ref config 'store-directory)
      (format #t "Please specify the --store-directory.~%")
      (exit 1))

    (unless (assoc-ref config 'report-bucket)
      (format #t "Please specify the --report-bucket.~%")
      (exit 1))

    (unless (assoc-ref config 'region)
      (format #t "Please specify the --region.~%")
      (exit 1))

    (log-debug "extract-region" "Started.")
    (log-error "extract-region" "Started.")
    (let* ((report-bucket   (assoc-ref config 'report-bucket))
           (store-directory (assoc-ref config 'store-directory))
           (region          (assoc-ref config 'region))
           (donors          (if (assoc-ref config 'donor-id)
                                (list (assoc-ref config 'donor-id))
                                (donors-from-bucket report-bucket))))
      (process-jobs donors
                    (string->number (assoc-ref config 'simultaneous-donors))
                    report-bucket
                    store-directory
                    region))))
