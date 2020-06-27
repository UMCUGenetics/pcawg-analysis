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

(define (bam->read-groups bam-file split-completed dest-dir donor-full-name)
  (if (file-exists? split-completed)
      #t
      (begin
        (mkdir-p dest-dir)
        (let* ((cmd (format #f "~a split -@ ~a -u /dev/null -f ~s ~s"
                            %samtools %threads
                            (string-append dest-dir "/"
                                           donor-full-name "_NA_%#.%.")
                            bam-file))
               (port         (open-input-pipe cmd))
               (split-output (get-string-all port)))
          (if (zero? (status:exit-val (close-pipe port)))
              (call-with-output-file split-completed
                (lambda (port)
                  (format port "~a" split-output)
                  #t))
              (begin
                (format #t "Splitting the BAM files in read groups failed with:~%~a~%"
                        split-output)
                #f))))))

(define (read-groups->fastq dest-dir fastq-dir donor-full-name)
  (let ((unmap-complete (string-append fastq-dir "/complete"))
        (split-files (scandir dest-dir
                              (lambda (file)
                                (string-suffix? ".bam" file)))))
    (mkdir-p fastq-dir)
    (format #t "Going to process:~%~{  - ~a~%~}~%" split-files)
    (not (any not (map
                   (lambda (file)
                     (format #t "Unmapping ~s.~%" file)
                     (let* ((parts          (string-split (basename file ".bam") #\_))
                            (lane           (list-ref parts 2))
                            (dest-filename  (format #f  "~a/~a_NA_S1_L~3,,,'0@a"
                                                    fastq-dir donor-full-name lane))
                            (part-complete  (string-append dest-filename ".complete")))
                       (if (file-exists? part-complete)
                           #t
                           (let* ((cmd          (format #f "~a sort -t ~a --tmpdir ~a -N ~a | ~a fastq -@ ~a - -1 ~a -2 ~a 2> ~a"
                                                        %sambamba %threads
                                                        (if (tmpdir) (tmpdir) "/tmp")
                                                        (string-append dest-dir "/" file)
                                                        %samtools %threads
                                                        (string-append dest-filename "_R1_001.fastq.gz")
                                                        (string-append dest-filename "_R2_001.fastq.gz")
                                                        (string-append dest-filename ".log")))
                                  (port         (open-input-pipe cmd))
                                  (unmap-output (get-string-all port)))
                             (if (zero? (status:exit-val (close-pipe port)))
                                 (call-with-output-file part-complete
                                     (lambda (port)
                                       (format port "~a" unmap-output)
                                       #t))
                                 (begin
                                   (format #t "Sorting and writing to FASTQ for ~s failed.~%" file)
                                   #f))))))
                   split-files)))))

(define (make-google-bucket donor-full-name)
  (let ((name (string-append "gs://run-"  (string-downcase donor-full-name) "-umc1")))
    (if (zero? (system (string-append
                        %gsutil " mb"
                        " -p " (google-project)
                        " -l " (google-region)
                        " -c STANDARD "
                        name)))
        name
        #f)))

(define (upload-to-the-conglomerates-daughter fastq-dir donor-full-name)
  (let ((bucket (make-google-bucket donor-full-name)))
    (if (not bucket)
        #f
        (not (any not (map (lambda (file)
                             (zero? (system
                                     (string-append
                                      %gsutil " cp "
                                      bucket "/aligner/samples/"
                                      donor-full-name "/" file))))
                           (scandir fastq-dir
                                    (lambda (file)
                                      (string-suffix? ".fastq.gz" file)))))))))

(define (bam->fastq file-data)
  (let* ((donor         (assoc-ref file-data 'donor-id))
         (specimen-type (assoc-ref file-data 'specimen-type))
         (file-id       (assoc-ref file-data 'file-id))
         (bam-file      (filename-by-file-data file-data))
         (basedir         (file-id-directory file-id))
         (dest-dir        (string-append basedir "/split"))
         (split-completed (string-append dest-dir "/complete"))
         (fastq-dir       (string-append basedir "/fastq"))
         (sample-type     specimen-type)
         (donor-full-name (string-append donor
                                         (if (eq? sample-type 'TUMOR) "T" "R"))))
    (cond
     [(not (download-file file-data))
      #f]
     [(not (bam->read-groups bam-file split-completed dest-dir donor-full-name))
      #f]
     [(not (read-groups->fastq dest-dir fastq-dir donor-full-name))
      #f]
     [(not (upload-to-the-conglomerates-daughter fastq-dir donor-full-name))
      #f])))

