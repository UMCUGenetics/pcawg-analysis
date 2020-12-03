(define-module (ui postprocess)
  #:use-module (ice-9 format)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 threads)
  #:use-module (logger)
  #:use-module (pcawg filesystem)
  #:use-module (pcawg tools)
  #:use-module (pcawg google)
  #:use-module (pcawg unmapped-reads)
  #:use-module (srfi srfi-1)

  #:export (do-postprocess))

(define program-options
  '((report-bucket          (single-char #\b) (value #t))
    (debug-log              (single-char #\d) (value #t))
    (error-log              (single-char #\e) (value #t))
    (simultaneous-donors    (single-char #\t) (value #t))
    (store-directory        (single-char #\s) (value #t))
    (help                   (single-char #\h) (value #f))))

(define (show-help)
  (for-each (lambda (line) (display line) (newline))
   '("Options for 'postprocess':"
     "  --report-bucket=ARG        -b  The patient report bucket to use."
     "  --debug-log                -d  Where to write the debug log."
     "  --error-log                -e  Where to write the error log."
     "  --simultaneous-donors=ARG  -t  Number of donors to process in parallel."
     "  --store-directory=ARG      -s  Where to store the data."
     "  --help                     -h  Show this message."))
  (exit 0))

(define (donor->unmapped-reads bucket storedir donor-id)

  (define (extract-unmapped suffix)
    (let* ((full-id     (string-append donor-id suffix))
           (input-file  (format #f "~a/~a-from-jar/~a/aligner/~a.bam"
                                bucket donor-id full-id full-id))
           (output-file (format #f "~a/~a_unmapped.bam" storedir full-id))
           (done-file   (format #f "~a/~a_unmapped.done" storedir full-id)))
      (log-debug "donor->unmapped-reads" "Reading file: ~s" input-file)
      (if (file-exists? done-file)
          #t
          (receive (success? message)
              (extract-unmapped-reads input-file output-file "bam" 20 #t)
            (if (not success?)
                (begin
                  (log-error "donor->unmapped-reads" "Error: ~s: ~s"
                             full-id message)
                  #f)
                (begin
                  (call-with-output-file done-file
                    (lambda (port) (format port "")))
                  (log-debug "donor->unmapped-reads" "Finished: ~s" full-id)
                  (log-debug "donor->unmapped-reads"
                   "~s has ~a reads without a coordinate, and ~a were found."
                   donor-id (car message) (cadr message))
                  #t))))))

  (extract-unmapped "T")
  (extract-unmapped "R"))

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
    output))

(define* (process-jobs donors number-of-jobs report-bucket store-directory
                       #:optional (threads '()))
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
          (process-jobs donors number-of-jobs report-bucket store-directory '()))))

   ;; Spawn multiple jobs.
   (else
    (let ((donor (car donors)))
      (process-jobs (cdr donors) number-of-jobs report-bucket store-directory
                    (cons (call-with-new-thread
                           (lambda _
                             (donor->unmapped-reads
                              report-bucket store-directory donor)))
                          threads))))))

(define (do-postprocess options)
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

    (log-debug "postprocess" "Started.")
    (log-error "postprocess" "Started.")
    (let ((donors (donors-from-bucket (assoc-ref config 'report-bucket))))
      (process-jobs donors
                    (string->number (assoc-ref config 'simultaneous-donors))
                    (assoc-ref config 'report-bucket)
                    (assoc-ref config 'store-directory)))))
