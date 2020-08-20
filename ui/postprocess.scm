(define-module (ui postprocess)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 threads)
  #:use-module (logger)
  #:use-module (pcawg filesystem)
  #:use-module (pcawg tools)
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

  (define (bam-uri id)
    (format #f "~a-from-jar/~a/aligner/~a.bam" bucket id id))

  (define (filter-command prefix flag)
    (format #f "~a view -b -f ~a > ~a_unmapped_flag-~a.bam" %samtools flag prefix flag))

  (define (extract-command id)
    (let ((done-file (format #f "~a/~a_unmapped.done" store-directory id)))
      (if (file-exists? done-file)
          (format #f "exit 0")
          ;; This Bash-specific construction streams the BAM's contents to
          ;; multiple samtools filters.  Because Guile's ‘system’ command
          ;; uses "sh", we need to wrap the command and send it to
          ;; "bash".
          (format #f "echo \"~a cat ~a | ~a >(~a) >(~a) >(~a) | ~a\" | ~a && touch ~a"
                  %gsutil (bam-uri id) %tee
                  (filter-command id 4)
                  (filter-command id 12)
                  (filter-command id 73)
                  (filter-command id 133)
                  %bash
                  done-file))))

  (log-debug "donor->unmapped-reads" "Extracting unmapped reads for ~s" donor-id)
  (let* ((out           (donor-directory donor-id))
         (tumor-id      (string-append donor-id "T"))
         (reference-id  (string-append donor-id "R"))
         (tumor-cmd     (extract-command tumor-id))
         (reference-cmd (extract-command reference-id)))
    (every zero? (n-par-map 2 system (list tumor-cmd reference-cmd)))))

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

    (unless (assoc-ref config 'simultaneous-donors)
      (cons `(simultaneous-donors . 1) config))

    (unless (assoc-ref config 'store-directory)
      (format #t "Please specify the --store-directory.~%")
      (exit 1))

    (unless (assoc-ref config 'report-bucket)
      (format #t "Please specify the --report-bucket.~%")
      (exit 1))

    (log-debug "postprocess" "Started.")
    (log-error "postprocess" "Started.")
    (let ((donors (donors-from-bucket (assoc-ref config 'report-bucket))))
      (n-par-for-each (assoc-ref config 'simultaneous-donors)
                      (lambda (id)
                        (donor->unmapped-reads
                         (assoc-ref config 'report-bucket)
                         (assoc-ref config 'store-directory)
                         id))
                      donors))))
