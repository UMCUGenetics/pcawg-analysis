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

(define (donor->unmapped-reads bucket store-directory donor-id)
  (let ((done-file (format #f "~a/~a_unmapped.done" store-directory donor-id)))
    (if (file-exists? done-file)
        #t
        (let* ((input-file  (format #f "~a/~a-from-jar/~aT/aligner/~aT.bam"
                                    bucket donor-id donor-id donor-id))
               (output-file (format #f "~a/~aT_unmapped.bam"
                                    store-directory donor-id))
               (input-port  (open-input-pipe
                             (format #f "~a cat ~a" %gsutil input-file))))
          (setvbuf input-port 'block (expt 2 16))
          (receive (success? message)
              (with-input-from-port input-port
                (lambda _
                  (extract-unmapped-reads "-" output-file "bam" 20 #t)))
            (if (not success?)
                (begin
                  (format #t "Error: ~a" message)
                  #f)
                #t))))))

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
      (n-par-for-each (string->number (assoc-ref config 'simultaneous-donors))
                      (lambda (id)
                        (donor->unmapped-reads
                         (assoc-ref config 'report-bucket)
                         (assoc-ref config 'store-directory)
                         id))
                      donors))))
