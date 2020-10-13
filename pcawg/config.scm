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

(define-module (pcawg config)
  #:use-module (oop goops)
  #:export (access-token
            donor-id
            cache-directory
            google-archive-bucket
            google-cmek-path
            google-project
            google-region
            google-report-bucket
            google-service-account
            pipeline-jar
            project-code
            set-access-token!
            set-cache-directory!
            set-donor-id!
            set-google-archive-bucket!
            set-google-cmek-path!
            set-google-project!
            set-google-region!
            set-google-report-bucket!
            set-google-service-account!
            set-pipeline-jar!
            set-project-code!
            set-simultaneous-donors!
            set-store-directory!
            set-tmpdir!
            simultaneous-donors
            storage-profile
            store-directory
            tmpdir))

;; The configuration singleton
;; ----------------------------------------------------------------------------

(define-class <pcawg-config> ()
  (project-code         #:init-value #f
                        #:getter get-project-code
                        #:setter set-project-code-private!)

  (donor-id             #:init-value #f
                        #:getter get-donor-id
                        #:setter set-donor-id-private!)

  (store-directory      #:init-value (string-append (getcwd) "/store")
                        #:getter get-store-directory
                        #:setter set-store-directory-private!)

  (tmpdir               #:init-value (getenv "TMPDIR")
                        #:getter get-tmpdir
                        #:setter set-tmpdir-private!)

  (cache-directory      #:init-value #f
                        #:getter get-cache-directory
                        #:setter set-cache-directory-private!)

  (storage-profile      #:init-value "collab"
                        #:getter get-storage-profile)

  (access-token         #:init-value (getenv "ACCESSTOKEN")
                        #:getter get-access-token
                        #:setter set-access-token-private!)

  (google-project       #:init-value #f
                        #:getter get-google-project
                        #:setter set-google-project-private!)

  (pipeline-jar         #:init-value #f
                        #:getter get-pipeline-jar
                        #:setter set-pipeline-jar-private!)

  (google-region        #:init-value #f
                        #:getter get-google-region
                        #:setter set-google-region-private!)

  (google-cmek-path     #:init-value #f
                        #:getter get-google-cmek-path
                        #:setter set-google-cmek-path-private!)

  (google-service-account #:init-value #f
                        #:getter get-google-service-account
                        #:setter set-google-service-account-private!)

  (google-archive-bucket #:init-value #f
                        #:getter get-google-archive-bucket
                        #:setter set-google-archive-bucket-private!)

  (google-report-bucket #:init-value #f
                        #:getter get-google-report-bucket
                        #:setter set-google-report-bucket-private!)

  (simultaneous-donors  #:init-value 2
                        #:getter get-simultaneous-donors
                        #:setter set-simultaneous-donors-private!))

(define %pcawg-config (make <pcawg-config>))

(define (make-getter symbol)
  (primitive-eval `(define (,symbol)
                     (,(symbol-append 'get- symbol)
                      %pcawg-config))))

(define (make-setter symbol)
  (primitive-eval `(define (,(symbol-append 'set- symbol '!) val)
                     (,(symbol-append 'set- symbol '-private!)
                      %pcawg-config val))))

(define (make-getter/setter symbol)
  (make-getter symbol)
  (make-setter symbol))

(make-getter 'storage-profile)
(for-each make-getter/setter '(donor-id
                               project-code
                               simultaneous-donors
                               store-directory
                               access-token
                               google-archive-bucket
                               google-service-account
                               google-cmek-path
                               google-project
                               google-region
                               google-report-bucket
                               pipeline-jar
                               tmpdir
                               cache-directory))
