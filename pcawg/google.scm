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
  #:use-module (json)
  #:use-module (pcawg tools)
  #:use-module (logger)

  #:export (may-run-pipeline-run?))

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
(define (make-availability-shorthand symbol identifier)
  (primitive-eval `(define-public (,(symbol-append 'available- symbol) region)
                     (quota-usage region ,identifier))))

;; This will generate the functions:
;; ‘available-cpus’, ‘available-preemptible-cpus’, etc..
(for-each (lambda (pair)
            (make-availability-shorthand (car pair) (cdr pair)))
          '((cpus             . "CPUS")
            (preemptible-cpus . "PREEMPTIBLE_CPUS")
            (disk-space       . "DISKS_TOTAL_GB")
            (ssd-space        . "SSD_TOTAL_GB")
            (local-ssd-space  . "LOCAL_SSD_TOTAL_GB")
            (n2d-cpus         . "N2D_CPUS")))

;; Advisory functions
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
