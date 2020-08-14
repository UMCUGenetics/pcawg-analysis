(define-module (ui panel-file)
  #:use-module (ice-9 getopt-long)
  #:use-module (pcawg hartwig)
  #:use-module (pcawg config)

  #:export (do-panel-file))

(define program-options
  '((store-directory        (single-char #\s) (value #t))
    (donor-name             (single-char #\d) (value #t))
    (help                   (single-char #\h) (value #f))))

(define (show-help)
  (for-each (lambda (line) (display line) (newline))
   '("Options for 'panel-file':"
     "  --store-directory=ARG      -s  Where to get the data."
     "  --donor-name=ARG           -d  Name of the donor."
     "  --help                     -h  Show this message."))
  (exit 0))

(define (do-panel-file options)
  (let* [(config     (getopt-long options program-options))
         (donor-name (assoc-ref config 'donor-name))]

    (when (assoc-ref config 'help)    (show-help))
    (when (assoc-ref config 'store-directory)
      (set-store-directory! (assoc-ref config 'store-directory)))

    (if donor-name
        (format #t "~a~%" (panel-file donor-name))
        (format #t "Please specify --donor-name.~%"))))

