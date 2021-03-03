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

(define-module (pcawg dcc-portal)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 match)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (srfi srfi-1)
  #:use-module (json)
  #:use-module (pcawg config)
  #:use-module (logger)

  #:export (metadata-for-project
            metadata-for-donor
            manifest-for-file-id
            donors-in-project
            files-for-donor
            normalize-specimen-type))

;; DCC Portal API convenience
;; ----------------------------------------------------------------------------

(define %base-uri "https://dcc.icgc.org/api/v1")

(define (manifest-for-file-id file-id)
  "Requests and returns a manifest token for FILE-ID, or #f otherwise."
  (call-with-values
    (lambda ()
      (http-post (string-append %base-uri "/manifests?filters="
                                (uri-encode
                                 (scm->json-string
                                  `((file (id (is . #(,file-id))))))))
                 #:headers
                 '((accept       . ((text/plain)))
                   (content-type . (application/x-www-form-urlencoded)))))
    (lambda (header body)
      (if (eq? (response-code header) 200)
          body
          #f))))

(define (normalize-specimen-type specimen-type)
  (cond
   ((string-contains-ci specimen-type "normal")
    'NORMAL)
   ((string-contains-ci specimen-type "tumour")
    'TUMOR)
   ((string-contains-ci specimen-type "tumor")
    'TUMOR)
   (else
    'UNKNOWN)))

(define (restructure-metadata metadata)
  (catch #t
    (lambda ()
      (map (lambda (item)
             (let ((donors     (vector->list (assoc-ref item "donors")))
                   (fileCopies (vector->list (assoc-ref item "fileCopies"))))
               `((donor-id      . ,(assoc-ref (car donors) "donorId"))
                 (file-id       . ,(assoc-ref item "id"))
                 (object-id     . ,(assoc-ref item "objectId"))
                 (file-size     . ,(assoc-ref (car fileCopies) "fileSize"))
                 (file-name     . ,(assoc-ref (car fileCopies) "fileName"))
                 (sample-id     . ,(car (vector->list
                                         (assoc-ref (car donors) "sampleId"))))
                 (specimen-type . ,(normalize-specimen-type
                                    (car
                                     (vector->list
                                      (assoc-ref (car donors)
                                                 "specimenType"))))))))
           (vector->list metadata)))
    (lambda (key . args)
      (log-error "restructure-metadata" "Unexpected metadata structure.")
      #f)))

(define (metadata-for-donor donor-id)
  (let ((cache-filename (format #f "~a/~a.json" (cache-directory) donor-id)))
    (if (and (cache-directory)
             (file-exists? cache-filename))
        (restructure-metadata (call-with-input-file cache-filename json->scm))
        (call-with-values
            (lambda ()
              (http-get (string-append
                         %base-uri "/repository/files/export?type=json&filters="
                         (uri-encode
                          (scm->json-string
                           `((file (donorId              (is . #(,donor-id)))
                                   (experimentalStrategy (is . #("WGS")))
                                   (study                (is . #("PCAWG")))
                                   (fileFormat           (is . #("BAM"))))))))
                        #:streaming? #t
                        #:headers '((accept . ((application/json))))))
          (lambda (header port)
            (if (eq? (response-code header) 200)
                (begin
                  (let ((data (json->scm port)))
                    (restructure-metadata data)))
                (begin
                  (log-error "metadata-for-donor"
                             "Response code was ~a"
                             (response-code header))
                  #f)))))))

(define (metadata-for-project project-code)
  (let ((cache-filename (format #f "~a/~a.json" (cache-directory) project-code)))
    (if (and (cache-directory)
             (file-exists? cache-filename))
        (restructure-metadata (call-with-input-file cache-filename json->scm))
        (call-with-values
            (lambda ()
              (http-get (string-append
                         %base-uri "/repository/files/export?type=json&filters="
                         (uri-encode
                          (scm->json-string
                           `((file (projectCode          (is . #(,project-code)))
                                   (experimentalStrategy (is . #("WGS")))
                                   (study                (is . #("PCAWG")))
                                   (fileFormat           (is . #("BAM"))))))))
                        #:streaming? #t
                        #:headers '((accept . ((application/json))))))
          (lambda (header port)
            (if (eq? (response-code header) 200)
                (begin
                  (let ((data (json->scm port)))
                    (restructure-metadata data)))
                (begin
                  (log-error "metadata-for-project"
                             "Response code was ~a"
                             (response-code header))
                  #f)))))))

(define (donors-in-project metadata)
  (delete-duplicates (map (lambda (item) (cdr (car item))) metadata) string=))

(define (files-for-donor donor-id metadata)
  (delete #f (map (lambda (item)
                    (if (and (string= (assoc-ref item 'donor-id) donor-id)
                             ;; A minimum filesize of 40G excludes the “mini”
                             ;; BAM files.
                             (> (assoc-ref item 'file-size) 40000000000))
                        item
                        #f))
                  metadata)))
