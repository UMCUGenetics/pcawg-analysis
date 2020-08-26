(define-module (ui remove-donor)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (pcawg hartwig)
  #:use-module (pcawg config)

  #:export (do-remove-donor))

(define program-options
  '((store-directory        (single-char #\s) (value #t))
    (donor-name             (single-char #\d) (value #t))
    (help                   (single-char #\h) (value #f))))

(define* (delete-directory dir #:key follow-mounts?)
  "Delete DIR recursively, like `rm -rf', without following symlinks.  Don't
follow mount points either, unless FOLLOW-MOUNTS? is true.  Report but ignore
errors."
  (let ((dev (stat:dev (lstat dir))))
    (file-system-fold (lambda (dir stat result)    ; enter?
                        (or follow-mounts?
                            (= dev (stat:dev stat))))
                      (lambda (file stat result)   ; leaf
                        (delete-file file))
                      (const #t)                   ; down
                      (lambda (dir stat result)    ; up
                        (rmdir dir))
                      (const #t)                   ; skip
                      (lambda (file stat errno result)
                        (format (current-error-port)
                                "warning: failed to delete ~a: ~a~%"
                                file (strerror errno)))
                      #t
                      dir

                      ;; Don't follow symlinks.
                      lstat)))

(define (show-help)
  (for-each (lambda (line) (display line) (newline))
   '("Options for 'remove-donor':"
     "  --store-directory=ARG      -s  Where to get the data."
     "  --donor-name=ARG           -d  Name of the donor."
     "  --help                     -h  Show this message."))
  (exit 0))

(define (remove-donor donor-name)
  (let ((directories '()))
    ;; Find the folders containing data for DONOR-NAME.
    ;; ------------------------------------------------------------------------
    (ftw (string-append (store-directory) "/files")
         (lambda (filename statinfo flag)
           (match flag
             ['regular
              (if (string-contains filename donor-name)
                  (let ((path (string-split filename #\/)))
                    (set! directories
                          (cons (dirname (dirname filename)) directories))
                    #t)
                  #t)]
             [_ #t])))

    ;; Remove the folders we found.
    ;; ------------------------------------------------------------------------
    (if (null? directories)
        (format #t "No data found.~%")
        (begin
          (delete-directory
           (string-append (store-directory) "/donors/" donor-name))
          (for-each delete-directory (delete-duplicates directories string=))
          (format #t "Data has been removed.~%")))))

(define (do-remove-donor options)
  (let* [(config     (getopt-long options program-options))
         (donor-name (assoc-ref config 'donor-name))]

    (when (assoc-ref config 'help)    (show-help))

    (if (assoc-ref config 'store-directory)
        (set-store-directory! (assoc-ref config 'store-directory))
        (begin
          (format #t "Please specify --store-directory.~%")
          (exit 0)))

    (if donor-name
        (format #t "~a~%" (remove-donor donor-name))
        (format #t "Please specify --donor-name.~%"))))
