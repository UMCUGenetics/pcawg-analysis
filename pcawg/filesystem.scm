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

(define-module (pcawg filesystem)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (json)
  #:use-module (pcawg config)
  #:use-module (pcawg dcc-portal)
  #:use-module (pcawg tools)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (web uri)

  #:export (mkdir-p
            file-id-directory
            filename-by-file-data
            donor-directory
            download-file
            bam->fastq))

;; Filesystem management
;; ----------------------------------------------------------------------------

(define (mkdir-p dir)
  "Create directory DIR and all its ancestors."
  (define absolute?
    (string-prefix? "/" dir))

  (define not-slash
    (char-set-complement (char-set #\/)))

  (let loop ((components (string-tokenize dir not-slash))
             (root       (if absolute?
                             ""
                             ".")))
    (match components
      ((head tail ...)
       (let ((path (string-append root "/" head)))
         (catch 'system-error
           (lambda ()
             (mkdir path)
             (loop tail path))
           (lambda args
             (if (= EEXIST (system-error-errno args))
                 (loop tail path)
                 (apply throw args))))))
      (() #t))))

(define (file-id-directory file-id)
  (let ((dest (string-append (store-directory) "/files/" file-id)))
    (mkdir-p dest)
    dest))

(define (donor-directory donor-id)
  (let ((dest (string-append (store-directory) "/donors/" donor-id)))
    (mkdir-p dest)
    dest))

(define (fastqs-for-file-id file-id)
  (let ((dest (string-append (file-id-directory file-id) "/fastq")))
    (mkdir-p dest)
    dest))

(define (filename-by-file-data file-data)
  (let* ((file-id     (assoc-ref file-data 'file-id))
         (object-id   (assoc-ref file-data 'object-id)))
    (if (and (string? file-id)
             (string? object-id))
        (string-append (file-id-directory file-id) "/" object-id ".bam")
        #f)))

(define (download-file file-data)
  "Downloads the file identified by FILE-DATA to its preconfigured destination."
  (let* ((file-id       (assoc-ref file-data 'file-id))
         (object-id     (assoc-ref file-data 'object-id))
         (expected-size (assoc-ref file-data 'file-size))
         (filename      (filename-by-file-data file-data)))
    (cond
     [(file-exists? (string-append filename ".complete"))
      #t]
     [(not object-id)
      (format #t "Couldn't get object ID for ~s.~%" file-id)
      #f]
     [else
      (let* ((cmd  (format #f "~a url --object-id ~s" %score-client object-id))
             (port (open-input-pipe cmd))
             (url  (read-line port)))
        (close-pipe port)
        (if (and (not (eof-object? url))
                 (uri? (string->uri url)))
            (let* ((cmd (format #f "~a --silent ~s --output ~s --write-out ~s"
                                %curl url filename "%{size_download}"))
                   (port (open-input-pipe cmd))
                   (bytes-received (string->number (read-line port))))
              (close-pipe port)
              (if (= bytes-received expected-size)
                  (call-with-output-file (string-append filename ".complete")
                    (lambda (port) (format port "~a" cmd) #t))
                  (begin
                    (format #t "Expected ~a bytes.~%Received ~a bytes.~%"
                            expected-size bytes-received)
                    #f)))
            (begin
              (format #t "Couldn't get URL for ~s.~%" file-id)
              #f)))])))

;; (define (socket-to-file input-port output-port)
;;   (let* ((buffer-size (expt 2 20))
;;          (buffer      (make-bytevector buffer-size))
;;          (received    (get-bytevector-n! input-port buffer 0 buffer-size))
;;          (total-recvd received))
;;     (while (not (eof-object? received))
;;       (put-bytevector output-port buffer 0 received)
;;       (set! received (get-bytevector-n! input-port buffer 0 buffer-size))
;;       (unless (eof-object? received)
;;         (set! total-recvd (+ total-recvd received))))
;;     total-recvd))

;; Our “internal cURL” implementation doesn't receive the entire BAM file.
;; So when re-activating this, please test it properly.
;; -----------------------------------------------------------------------
;; (call-with-values
;;   (lambda _ (http-get url #:streaming? #t))
;;   (lambda (header input-port)
;;     (if (= (response-code header) 200)
;;         (let* ((bytes       (response-content-length header))
;;                (filename    (string-append (file-id-directory file-id)
;;                                            "/" object-id ".bam"))
;;                (output-port (open-output-file filename  #:binary #t))
;;                (received    (socket-to-file input-port output-port)))
;;           (if (= bytes received)
;;               (begin
;;                 (close-port output-port)
;;                 #t)
;;               (begin
;;                 (format #t "Download for ~s is incomplete.~%" file-id)
;;                 #f))))))
;; #f)))))

(define (bam->fastq file-data)
  (let ((donor         (assoc-ref file-data 'donor-id))
        (specimen-type (assoc-ref file-data 'specimen-type))
        (file-id       (assoc-ref file-data 'file-id))
        (bam-file      (filename-by-file-data file-data)))
    (if (not (file-exists? bam-file))
        (begin
          (format #t "Please download ~a first.~%" file-id)
          #f)
        (let* ((basedir         (file-id-directory file-id))
               (dest-dir        (string-append basedir "/split"))
               (split-completed (string-append dest-dir "/complete"))
               (fastq-dir       (string-append basedir "/fastq"))
               (sample-type     specimen-type))
          (mkdir-p dest-dir)
          (unless (file-exists? split-completed)
            (let* ((cmd (format #f "~a split -@ ~a -u /dev/null -f ~s ~s"
                                %samtools %threads
                                (string-append dest-dir "/" donor
                                  (if (eq? sample-type 'TUMOR) "T" "R")
                                  "_NA_%!_%#.%.")
                                bam-file))
                   (port         (open-input-pipe cmd))
                   (split-output (get-string-all port)))
              (if (zero? (status:exit-val (close-pipe port)))
                  (call-with-output-file (string-append dest-dir "/complete")
                    (lambda (port)
                      (format port "~a" split-output)))
                  (begin
                    (format #t "Splitting the BAM files in read groups failed with:~%~a~%"
                            split-output)
                    #f))))
          (if (not (file-exists? split-completed))
              #f
              (let ((split-files (scandir dest-dir
                                          (lambda (file)
                                            (string-suffix? file ".bam")))))
                (for-each
                 (lambda (file)
                   (let* ((parts          (string-split (basename file ".bam") "_"))
                          (donor          (list-ref parts 0))
                          (flowcell       (list-ref parts 1))
                          (index          (list-ref parts 2))
                          (lane           (list-ref parts 3))
                          (dest-filename  (format #f  "~a/~a_~a_S~a_L~3,,,'0@a"
                                                  fastq-dir donor flowcell index lane)))
                     (if (zero? (system
                                 (string-append
                                  %samtools " sort -@ "  %threads " -T " (tmpdir) " -n " dest-dir "/" file " | "
                                  %samtools " fastq -@ " %threads " - "
                                  " -1 " dest-filename "_R1_001.fastq.gz"
                                  " -2 " dest-filename "_R2_001.fastq.gz")))
                         #t
                         (begin
                           (format #t "Sorting and writing to FASTQ for ~s failed.~%" file)
                           #f))))
                 split-files)))))))

(define (make-google-buckets donor-id)
  (let ((command (lambda (tumor?)
                   (string-append
                    (gsutil) "mb"
                    " -p " (google-project)
                    " -l " (google-region)
                    " -c STANDARD "
                    "gs://run-"  (string-downcase donor-id)
                                 (if tumor? "t" "r")
                                 "-umc1"))))
    (system (command #f))
    (system (command #t))))
