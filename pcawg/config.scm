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
            cache-directory
            google-project
            google-region
            google-sdk
            project-code
            set-access-token!
            set-cache-directory!
            set-google-project!
            set-google-region!
            set-google-sdk!
            set-project-code!
            set-store-directory!
            set-tmpdir!
            storage-profile
            store-directory
            tmpdir))

;; The configuration singleton
;; ----------------------------------------------------------------------------

(define-class <pcawg-config> ()
  (project-code    #:init-value #f
                   #:getter get-project-code
                   #:setter set-project-code-private!)
  (store-directory #:init-value (string-append (getcwd) "/store")
                   #:getter get-store-directory
                   #:setter set-store-directory-private!)
  (tmpdir          #:init-value (getenv "TMPDIR")
                   #:getter get-tmpdir
                   #:setter set-tmpdir-private!)
  (cache-directory #:init-value #f
                   #:getter get-cache-directory
                   #:setter set-cache-directory-private!)
  (storage-profile #:init-value "collab"
                   #:getter get-storage-profile)
  (access-token    #:init-value (getenv "ACCESSTOKEN")
                   #:getter get-access-token
                   #:setter set-access-token-private!)
  (google-sdk      #:init-value #f
                   #:getter get-google-sdk
                   #:setter set-google-sdk-private!)
  (google-project  #:init-value #f
                   #:getter get-google-project
                   #:setter set-google-project-private!)
  (google-region  #:init-value #f
                   #:getter get-google-region
                   #:setter set-google-region-private!))

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
(for-each make-getter/setter '(project-code
                               store-directory
                               access-token
                               google-sdk
                               google-region
                               google-project
                               tmpdir
                               cache-directory))
