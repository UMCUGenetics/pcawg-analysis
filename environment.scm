;; Copyright (C) 2020  Roel Janssen

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(use-modules (guix packages)
             ((guix licenses) #:prefix license:)
             (gnu packages admin)
             (gnu packages autotools)
             (gnu packages bioinformatics)
             (gnu packages certs)
             (gnu packages compression)
             (gnu packages curl)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages java)
             (gnu packages pkg-config)
             (gnu packages tls)
             (gnu packages)
             (guix build utils)
             (guix build-system gnu)
             (guix download)
             (guix packages)
             (umcu packages bioinformatics))

(define pcawg-analysis
  (package
   (name "pcawg-analysis")
   (version "0.0.7")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/UMCUGenetics/pcawg-analysis/"
                  "releases/download/" version "/pcawg-analysis-"
                  version ".tar.gz"))
            (sha256
             (base32
              "0f3gzvgd8kp420czx4km8m8v8gandakshhwkzsrxjrdzsysq6s5d"))))
   (build-system gnu-build-system)
   (arguments
    `(#:modules ((guix build gnu-build-system)
                 ((guix build guile-build-system)
                  #:select (target-guile-effective-version))
                 (guix build utils))
      #:imported-modules ((guix build guile-build-system)
                          ,@%gnu-build-system-modules)
      #:phases
      (modify-phases %standard-phases
        (add-after 'install 'wrap-executable
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let* ((out  (assoc-ref outputs "out"))
                   (guile-version (target-guile-effective-version))
                   (guile-load-path
                    (string-append out "/share/guile/site/"
                                   guile-version ":"
                                   (getenv "GUILE_LOAD_PATH")))
                   (guile-load-compiled-path
                    (string-append out "/lib/guile/"
                                   guile-version "/site-ccache:"
                                   (getenv "GUILE_LOAD_COMPILED_PATH")))
                   (web-root (string-append
                              out "/share/sparqling-genomics/web"))
                   (certs (assoc-ref inputs "nss-certs"))
                   (certs-dir (string-append certs "/etc/ssl/certs")))
              (wrap-program (string-append out "/bin/acontrol")
                `("GUILE_LOAD_PATH" ":" prefix (,guile-load-path))
                `("GUILE_LOAD_COMPILED_PATH" ":" prefix
                  (,guile-load-compiled-path))
                `("SSL_CERT_DIR" ":" = (,certs-dir))
                `("_JAVA_OPTIONS" ":" = (,(string-append "-Djavax.net.ssl.trustStore="
                                                         (assoc-ref inputs "openjdk")
                                                         "/lib/security/cacerts"))))))))))
   (native-inputs
    `(("autoconf" ,autoconf)
      ("automake" ,automake)
      ("libtool" ,libtool)
      ("pkg-config" ,pkg-config)))
   (inputs
    `(("guile" ,guile-3.0-latest)
      ("guile-json" ,guile-json-4)
      ("gnutls" ,gnutls)
      ("htslib" ,htslib-with-gcs-support)
      ("nss-certs" ,nss-certs)
      ("samtools" ,samtools)
      ("sambamba" ,sambamba)
      ("score-client" ,score-client)
      ("google-cloud-sdk" ,google-cloud-sdk)
      ("openjdk" ,openjdk11)
      ("curl" ,curl)))
   (propagated-inputs
    `(("net-base" ,net-base))) ; This is a “fix” for the Docker container.
   (home-page #f)
   (synopsis "Tools to “do” the PCAWG analysis.")
   (description "This package is purely used for a personal task.")
   (license license:gpl3+)))

pcawg-analysis
