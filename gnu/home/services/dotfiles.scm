;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2024 Giacomo Leidi <goodoldpaul@autistici.org>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu home services dotfiles)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:autoload   (guix build utils) (find-files)
  #:use-module (guix deprecation)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module ((guix utils) #:select (current-source-directory source-properties->location))
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:export (home-dotfiles-service-type
            home-dotfiles-configuration->files
            home-dotfiles-environment->files

            home-dotfiles-configuration
            home-dotfiles-configuration?
            home-dotfiles-configuration-fields
            home-dotfiles-configuration-layout
            home-dotfiles-configuration-source-directory
            home-dotfiles-configuration-packages
            home-dotfiles-configuration-directories
            home-dotfiles-configuration-excluded

            home-dotfiles-environment
            home-dotfiles-environment?
            home-dotfiles-environment-fields
            home-dotfiles-environment-source-directory
            home-dotfiles-environment-directories
            home-dotfiles-environment-excluded

            stow-dotfiles-directory
            stow-dotfiles-directory?
            stow-dotfiles-directory-fields
            stow-dotfiles-directory-name
            stow-dotfiles-directory-packages

            plain-dotfiles-directory
            plain-dotfiles-directory?
            plain-dotfiles-directory-fields
            plain-dotfiles-directory-name))

(define %home-dotfiles-excluded
  '(".*~"
    ".*\\.swp"
    "\\.git/.*"
    "\\.gitignore"))

(define (sanitize-layout value)
  (if (member value '(plain stow))
      value
      (raise
       (formatted-message
        (G_ "layout field of home-dotfiles-configuration should be either
'plain or 'stow, but ~a was found.")
        value))))

(define list-of-strings?
  (list-of string?))

(define-maybe list-of-strings)

(define-configuration/no-serialization home-dotfiles-configuration
  (source-directory
   (string (current-source-directory))
   "The path where dotfile directories are resolved.  By default dotfile
directories are resolved relative the source location where
@code{home-dotfiles-configuration} appears.")
  (layout
   (symbol 'plain)
   "The intended layout of the specified @code{directory}.  It can be either
@code{'stow} or @code{'plain}."
   (sanitizer sanitize-layout))
  (directories
   (list-of-strings '())
   "The list of dotfiles directories where @code{home-dotfiles-service-type}
will look for application dotfiles.")
  (packages
   (maybe-list-of-strings)
   "The names of a subset of the GNU Stow package layer directories.  When provided
the @code{home-dotfiles-service-type} will only provision dotfiles from this
subset of applications.  This field will be ignored if @code{layout} is set
to @code{'plain}.")
  (excluded
   (list-of-strings '(".*~" ".*\\.swp" "\\.git" "\\.gitignore"))
   "The list of file patterns @code{home-dotfiles-service-type} will exclude
while visiting @code{directory}."))

(define-configuration/no-serialization plain-dotfiles-directory
  (name
   (string)
   "The path of the dotfiles directory where @code{home-dotfiles-service-type}
will look for application dotfiles."))

(define-configuration/no-serialization stow-dotfiles-directory
  (name
   (string)
   "The path of the dotfiles directory where @code{home-dotfiles-service-type}
will look for application dotfiles.")
  (packages
   (maybe-list-of-strings)
   "The names of a subset of the GNU Stow package layer directories.  When provided
the @code{home-dotfiles-service-type} will only provision dotfiles from this
subset of applications."))

(define (list-of-dotfiles-directories? value)
  (map
   (lambda (record)
     (if (or (plain-dotfiles-directory? record)
             (stow-dotfiles-directory? record))
         value
         (raise
          (formatted-message
           (G_ "directories field of home-dotfiles-environment should be either a
plain-dotfiles-directory or stow-dotfiles-directory record, but ~a was found.")
           record))))
   value))

(define-configuration/no-serialization home-dotfiles-environment
  (source-directory
   (string (current-source-directory))
   "The path where dotfile directories are resolved.  By default dotfile
directories are resolved relative the source location where
@code{home-dotfiles-environment} appears.")
  (directories
   (list-of-dotfiles-directories '())
   "The list of dotfiles directories where @code{home-dotfiles-service-type}
will look for application dotfiles.")
  (excluded
   (list-of-strings %home-dotfiles-excluded)
   "The list of file patterns @code{home-dotfiles-service-type} will exclude
while visiting each one of the @code{directories}."))

(define (strip-stow-dotfile file-name directory)
  (let ((dotfile-name (string-drop file-name (1+ (string-length directory)))))
    (match (string-split dotfile-name #\/)
      ((package parts ...)
       (string-join parts "/")))))

(define (strip-plain-dotfile file-name directory)
  (string-drop file-name (+ 1 (string-length directory))))

(define (import-dotfiles directory files strip)
  "Return a list of objects compatible with @code{home-files-service-type}'s
value.  Each object is a pair where the first element is the relative path
of a file and the second is a gexp representing the file content.  Objects are
generated by recursively visiting DIRECTORY and mapping its contents to the
user's home directory, excluding files that match any of the patterns in EXCLUDED."
  (define (format file)
    ;; Remove from FILE characters that cannot be used in the store.
    (string-append
     "home-dotfiles-"
     (string-map (lambda (chr)
                   (if (and (char-set-contains? char-set:ascii chr)
                            (char-set-contains? char-set:graphic chr)
                            (not (memv chr '(#\. #\/ #\space))))
                       chr
                       #\-))
                 file)))

  (map (lambda (file)
         (let ((stripped (strip file directory)))
           (list stripped
                 (local-file file (format stripped)
                             #:recursive? #t))))
       files))

;; This procedure exists only to avoid the deprecation
;; warning when compiling home-dotfiles-service-files.
;; Once the deprecation period is over this internal procedure
;; can be removed, together with home-dotfiles-service-files
;; and home-dotfiles-configuration->files.
(define (home-dotfiles-configuration->files/internal config)
  (warning (G_ "'~a' is deprecated, use '~a' instead~%")
           'home-dotfiles-configuration 'home-dotfiles-environment)
  (define stow? (eq? (home-dotfiles-configuration-layout config) 'stow))
  (define excluded
    (home-dotfiles-configuration-excluded config))
  (define exclusion-rx
    (make-regexp (string-append "^.*(" (string-join excluded "|") ")$")))

  (define* (directory-contents directory #:key (packages #f))
    (define (filter-files directory)
      (find-files directory
                  (lambda (file stat)
                    (not (regexp-exec exclusion-rx
                                      (basename file))))))
    (if (and stow? packages (maybe-value-set? packages))
        (append-map filter-files
                    (map (lambda (pkg)
                           (string-append directory "/" pkg))
                         packages))
        (filter-files directory)))

  (define (resolve directory)
    ;; Resolve DIRECTORY relative to the 'source-directory' field of CONFIG.
    (if (string-prefix? "/" directory)
        directory
        (in-vicinity (home-dotfiles-configuration-source-directory config)
                     directory)))

  (append-map (lambda (directory)
                (let* ((directory (resolve directory))
                       (packages
                        (home-dotfiles-configuration-packages config))
                       (contents
                        (directory-contents directory
                                            #:packages packages))
                       (strip
                        (if stow? strip-stow-dotfile strip-plain-dotfile)))
                  (import-dotfiles directory contents strip)))
              (home-dotfiles-configuration-directories config)))

(define-deprecated (home-dotfiles-configuration->files config)
  home-dotfiles-environment->files
  (home-dotfiles-configuration->files/internal config))

(define (home-dotfiles-environment->files config)
  "Return a list of objects compatible with @code{home-files-service-type}'s
value, excluding files that match any of the patterns configured."
  (define excluded
    (home-dotfiles-environment-excluded config))
  (define exclusion-rx
    (make-regexp (string-append "^.*(" (string-join excluded "|") ")$")))

  (define* (directory-contents directory #:key (stow? #f) (packages #f))
    (define (filter-files directory)
      (find-files directory
                  (lambda (file stat)
                    (not (regexp-exec exclusion-rx file)))))
    (if (and stow? packages (maybe-value-set? packages))
        (append-map filter-files
                    (map (lambda (pkg)
                           (string-append directory "/" pkg))
                         packages))
        (filter-files directory)))

  (define (resolve directory)
    ;; Resolve DIRECTORY relative to the 'source-directory' field of CONFIG.
    (if (string-prefix? "/" directory)
        directory
        (in-vicinity (home-dotfiles-environment-source-directory config)
                     directory)))

  (append-map (lambda (record)
                (let* ((stow? (stow-dotfiles-directory? record))
                       (name
                        (if stow?
                            (stow-dotfiles-directory-name record)
                            (plain-dotfiles-directory-name record)))
                       (directory (resolve name))
                       (packages
                        (and stow?
                             (stow-dotfiles-directory-packages record)))
                       (contents
                        (directory-contents directory
                                            #:stow? stow?
                                            #:packages packages))
                       (strip
                        (if stow? strip-stow-dotfile strip-plain-dotfile)))
                  (import-dotfiles directory contents strip)))
              (home-dotfiles-environment-directories config)))

(define (home-dotfiles-service-files config)
  (if (home-dotfiles-environment? config)
      (home-dotfiles-environment->files config)
      (home-dotfiles-configuration->files/internal config)))

(define-public home-dotfiles-service-type
  (service-type (name 'home-dotfiles)
                (extensions
                 (list (service-extension home-files-service-type
                                          home-dotfiles-service-files)))
                (default-value (home-dotfiles-environment))
                (description "Files that will be put in the user's home directory
following GNU Stow's algorithm, and further processed during activation.")))
