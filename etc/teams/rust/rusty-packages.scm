;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Efraim Flashner <efraim@flashner.co.il>
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

;;; This file returns a manifest of packages built using the cargo-build-system
;;; which are NOT prefixed with 'rust-' and the packages which use rust itself
;;; as an input.  This is a short list of packages which can be checked to see
;;; if a rust update has gone smoothly.  It is used to assist continuous
;;; integration of the rust-team branch.

(use-modules (guix packages)
             (guix profiles)
             (guix build-system)
             (srfi srfi-1))

(manifest
  (map package->manifest-entry
       (fold-packages
         (lambda (package lst)
           (if (or
                 (and (eq? (build-system-name (package-build-system package))
                           (quote cargo))
                      (not (string-prefix? "rust-" (package-name package))))
                 (any
                   (lambda (pkg)
                     (member (specification->package "rust") pkg))
                   (append (package-native-inputs package)
                           (package-propagated-inputs package)
                           (package-inputs package))))
             (cons package lst)
             lst))
         (list))))
