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

(define-module (pcawg google)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (json)
  #:use-module (pcawg tools)
  #:use-module (logger)
  #:use-module (web client)
  #:use-module (web response)

  #:export (available-cpus
            available-preemptible-cpus
            available-disk-space
            available-ssd-space
            available-local-ssd-space
            available-n2d-cpus

            bucket-exists?
            may-run-pipeline-run?

            get-token
            token-expires-in))

;; OAuth token management.
;; ----------------------------------------------------------------------------

(define (get-token)
  (let ((token (getenv "GCS_OAUTH_TOKEN")))
    (if (and token (token-expires-in token))
        token
        (let* ((cmd   (string-append %gcloud
                       " auth application-default print-access-token"))
               (port  (open-input-pipe cmd))
               (token (read-line port)))
          (close-pipe port)
          (setenv "GCS_OAUTH_TOKEN" token)
          token))))

(define (token-expires-in token)
  "Return the number of seconds that TOKEN is valid."
  (if token
      (catch #t
        (lambda _
          (call-with-values
              (lambda _
                (http-get (string-append
                           "https://www.googleapis.com/oauth2/v1"
                           "/tokeninfo?access_token=" token "")
                          #:headers    '((accept . ((application/json))))
                          #:streaming? #t))
            (lambda (header port)
              (if (eq? (response-code header) 200)
                  (let ((data (json->scm port)))
                    (assoc-ref data "expires_in"))
                  #f))))
        (lambda (key . args)
          #f))
      #f))

;; Quota management.
;; ----------------------------------------------------------------------------
(define (quota-usage region property)
  (catch #t
    (lambda _
      (let* ((cmd    (string-append %gcloud " compute regions describe "
                                    region " --format json"))
             (port   (open-input-pipe cmd))
             (data   (json->scm port))
             (quotas (vector->list (assoc-ref data "quotas"))))
        (car (delete #f (map (lambda (item)
                               (if (string= (assoc-ref item "metric") property)
                                   `((usage . ,(assoc-ref item "usage"))
                                     (limit . ,(assoc-ref item "limit")))
                                   #f))
                             quotas)))))
    (lambda (key . args)
      (log-error "quota-usage" "Error: ~a: ~s" key args)
      '())))

;; Convenience methods for quota management.
;; ----------------------------------------------------------------------------
;; This will generate the functions:
;; ‘available-cpus’, ‘available-preemptible-cpus’, etc..

(for-each (lambda (pair)
            (primitive-eval
             `(define (,(symbol-append 'available- (car pair)) region)
                (quota-usage region ,(cdr pair)))))
          '((cpus             . "CPUS")
            (preemptible-cpus . "PREEMPTIBLE_CPUS")
            (disk-space       . "DISKS_TOTAL_GB")
            (ssd-space        . "SSD_TOTAL_GB")
            (local-ssd-space  . "LOCAL_SSD_TOTAL_GB")
            (n2d-cpus         . "N2D_CPUS")))

;; Advisory functions.
;; ----------------------------------------------------------------------------
(define (may-run-pipeline-run? region lanes cpus-per-lane)
  (catch #t
    (lambda _
      (let* ((data (available-preemptible-cpus region))
             (used      (assoc-ref data 'usage))
             (limit     (assoc-ref data 'limit))
             (available (- limit used)))
        (> available (* lanes cpus-per-lane))))
    (lambda (key . args)
      #f)))

;; Other utils.
;; ----------------------------------------------------------------------------

(define (bucket-exists? bucket)
  (zero?
   (system
    (string-append %gsutil " ls " bucket " > /dev/null 2> /dev/null"))))
