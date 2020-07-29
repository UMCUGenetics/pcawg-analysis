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
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 threads)
  #:use-module (json)
  #:use-module (pcawg config)
  #:use-module (pcawg dcc-portal)
  #:use-module (pcawg tools)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (logger)

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
      (log-error "download-file" "Couldn't get object ID for ~s." file-id)
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
                    (log-error "download-file"
                               "Expected ~a bytes.~%Received ~a bytes for ~s."
                               expected-size bytes-received object-id)
                    #f)))
            (begin
              (log-error "download-file" "Couldn't get URL for ~s." object-id)
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
                  (format port "Deleting ~s.~%" bam-file)
                  (delete-file bam-file)
                  (format port "~a" split-output)
                  #t))
              (begin
                (log-error "bam->read-groups"
                           "Splitting the BAM files in read groups failed with:~%~a"
                           split-output)
                #f))))))

(define (read-groups->fastq dest-dir fastq-dir donor-full-name)
  (let ((unmap-complete (string-append fastq-dir "/complete"))
        (split-files (scandir dest-dir
                              (lambda (file)
                                (string-suffix? ".bam" file)))))
    (mkdir-p fastq-dir)
    (log-debug "read-groups->fastq"
               "Going to process:~%~{  - ~a~%~}"
               split-files)
    (not (any not
              (map
               (lambda (file)
                 (log-debug "read-groups->fastq" "Unmapping ~s." file)
                 (let* ((parts          (string-split (basename file ".bam") #\_))
                        (lane           (list-ref parts 2))
                        (dest-filename  (format #f  "~a/~a_NA_S1_L~3,,,'0@a"
                                                fastq-dir donor-full-name lane))
                        (part-complete  (string-append dest-filename ".complete")))
                   (if (file-exists? part-complete)
                       #t
                       (let* ((input-file   (string-append dest-dir "/" file))
                              (cmd          (format #f "~a sort -t ~a --tmpdir ~a -N ~a -o /dev/stdout | ~a fastq -@ ~a - -1 ~a -2 ~a 2> ~a"
                                                    %sambamba %threads
                                                    (if (tmpdir) (tmpdir) "/tmp")
                                                    input-file
                                                    %samtools %threads
                                                    (string-append dest-filename "_R1_001.fastq.gz")
                                                    (string-append dest-filename "_R2_001.fastq.gz")
                                                    (string-append dest-filename ".log")))
                              (port         (open-input-pipe cmd))
                              (unmap-output (get-string-all port)))
                         (if (zero? (status:exit-val (close-pipe port)))
                             (call-with-output-file part-complete
                               (lambda (port)
                                 (format port "Deleting ~s.~%" input-file)
                                 (delete-file input-file)
                                 (format port "~a" unmap-output)
                                 #t))
                             (begin
                               (log-debug "read-groups->fastq"
                                          "Sorting and writing to FASTQ for ~s failed."
                                          file))
                               #f))))))
               split-files)))))

(define (name-google-bucket donor-full-name)
  (string-append "gs://run-" (string-downcase donor-full-name) "-umc1"))

(define (make-google-bucket donor-full-name)
  (let ((name (name-google-bucket donor-full-name)))
    (if (zero? (system (string-append
                        %gsutil " mb"
                        " -p " (google-project)
                        " -l " (google-region)
                        " -c STANDARD "
                        name)))
        name
        #f)))

(define (bucket-exists? bucket)
  (zero?
   (system
    (string-append %gsutil " ls " bucket " > /dev/null 2> /dev/null"))))

(define (upload-to-the-conglomerates-daughter fastq-dir donor-full-name)
  (let ((bucket (make-google-bucket donor-full-name)))
    (if (not bucket)
        #f
        (not (any not
                  (map (lambda (file)
                         (let* ((name (string-append fastq-dir "/" file))
                                (comp (string-append name ".uploaded")))
                           (if (file-exists? comp)
                               #t
                               (let* ((cmd  (string-append
                                             %gsutil " cp " name " " bucket
                                             "/aligner/samples/"
                                             donor-full-name "/" file))
                                      (port (open-input-pipe cmd))
                                      (out  (get-string-all port)))
                                 (if (zero? (status:exit-val (close-pipe port)))
                                     (call-with-output-file comp
                                       (lambda (port)
                                         (format port "Deleting ~s.~%" name)
                                         (delete-file name)
                                         (format port "~a" out)
                                         #t))
                                     (begin
                                       (log-debug "upload-to-the-conglomerates-daughter"
                                                  "Uploading ~s failed." name)))))))
                       (scandir fastq-dir
                                (lambda (file)
                                  (string-suffix? ".fastq.gz" file)))))))))

(define (lanes-for-sample sample-name)
  (catch #t
    (lambda _
      (let* ((port (open-input-pipe
                    (string-append %gsutil
                     " ls gs://run-" (string-downcase sample-name)
                     "-umc1/aligner/samples/" (string-upcase sample-name)
                     "/*R1_001.fastq.gz")))
             (out  (get-string-all port)))
        (if (zero? (status:exit-val (close-pipe port)))
            (let* ((sorted (sort (string-split out #\newline) string<))
                   (lanes  (if (string= (car sorted) "") (cdr sorted) sorted)))
              (map (lambda (file)
                     ;; This whole construct assumes that the file is
                     ;; formatted correctly.  That's why we wrapped this fine
                     ;; piece in a catch construct.
                     (let* ((parts       (string-split (basename file) #\_))
                            (lane        (list-ref parts 3))
                            (lane-number (string->number (substring lane 1)))
                            (r1          (substring file 4))
                            (r2          (string-replace-substring r1 "R1" "R2")))
                       `((laneNumber       . ,(number->string lane-number))
                         (firstOfPairPath  . ,r1)
                         (secondOfPairPath . ,r2))))
                   lanes))
            #f)))
    (lambda (key . args)
      #f)))

(define (panel-file donor-name)
  (let ((filename  (string-append (donor-directory donor-name) "/panel.json"))
        (tumor     (string-append donor-name "T"))
        (reference (string-append donor-name "R")))
    (call-with-output-file filename
      (lambda (port)
        (scm->json port `((tumor     . ((type  . "TUMOR")
                                        (name  . ,tumor)
                                        (lanes . ,(lanes-for-sample tumor))))
                          (reference . ((type  . "REFERENCE")
                                        (name  . ,reference)
                                        (lanes . ,(lanes-for-sample reference))))))))
    filename))

(define (start-pipeline donor-name)
  (let ((reference-bucket (name-google-bucket (string-append donor-name "R")))
        (tumor-bucket     (name-google-bucket (string-append donor-name "T"))))
    (cond
     [(and (bucket-exists? reference-bucket)
           (bucket-exists? tumor-bucket))
      (let* ((logfile (lambda (file)
                        (string-append
                         (donor-directory donor-name) "/" file)))
             (port (open-input-pipe
                    (string-append
                     %java " -jar " (pipeline-jar)
                           " -profile development"
                           " -set_id " donor-name
                           " -run_id from-jar"
                           " -sample_json " (panel-file donor-name)
                           " -cloud_sdk " (string-drop-right
                                           (dirname %gcloud) 4)
                           " > " (logfile "/pipeline5.log")
                           " 2> "(logfile "/pipeline5.errors")))))
        (zero? (status:exit-val (close-pipe port))))]
     [else
      #f])))

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
      #f]
     [(not (start-pipeline donor))
      #f])))
