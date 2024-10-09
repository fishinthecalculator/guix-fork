;;; GNU Guix --- Functional package management for GNU
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

(define-module (gnu tests containers)
  #:use-module (gnu)
  #:use-module (gnu tests)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages containers)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu services)
  #:use-module (gnu services containers)
  #:use-module (gnu services docker)
  #:use-module (gnu services desktop)
  #:use-module (gnu services dbus)
  #:use-module (gnu services networking)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu system vm)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module ((guix scripts pack) #:prefix pack:)
  #:use-module (guix store)
  #:export (%test-rootless-podman
            %test-oci-container
            %test-oci-service-docker
            %test-oci-service-podman))


(define %rootless-podman-os
  (simple-operating-system
   (service rootless-podman-service-type
            (rootless-podman-configuration
             (subgids
              (list (subid-range (name "dummy"))))
             (subuids
              (list (subid-range (name "dummy"))))))

   (service dhcp-client-service-type)
   (service dbus-root-service-type)
   (service polkit-service-type)
   (service elogind-service-type)

   (simple-service 'accounts
                   account-service-type
                   (list (user-account
                          (name "dummy")
                          (group "users")
                          (supplementary-groups '("wheel" "netdev" "cgroup"
                                                  "audio" "video")))))))

(define (run-rootless-podman-test oci-tarball)

  (define os
    (marionette-operating-system
     (operating-system-with-gc-roots
      %rootless-podman-os
      (list oci-tarball))
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (volatile? #f)
     (memory-size 1024)
     (disk-image-size (* 3000 (expt 2 20)))
     (port-forwardings '())))

  (define test
    (with-imported-modules '((gnu build marionette)
                             (gnu services herd))
      #~(begin
          (use-modules (srfi srfi-11) (srfi srfi-64)
                       (gnu build marionette))

          (define marionette
            ;; Relax timeout to accommodate older systems and
            ;; allow for pulling the image.
            (make-marionette (list #$vm) #:timeout 60))
          (define out-dir "/tmp")

          (test-runner-current (system-test-runner #$output))
          (test-begin "rootless-podman")

          (test-assert "service started"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'cgroups2-fs-owner)
                  (#f #f)
                  ;; herd returns (running #f), likely because of one shot,
                  ;; so consider any non-error a success.
                  (('service response-parts ...) #t)))
             marionette))

          (test-equal "/sys/fs/cgroup/cgroup.subtree_control content is sound"
            (list "cpu" "cpuset" "io" "memory" "pids")
            (marionette-eval
             `(begin
                (use-modules (srfi srfi-1)
                             (ice-9 popen)
                             (ice-9 match)
                             (ice-9 rdelim))

                (define (read-lines file-or-port)
                  (define (loop-lines port)
                    (let loop ((lines '()))
                      (match (read-line port)
                        ((? eof-object?)
                         (reverse lines))
                        (line
                         (loop (cons line lines))))))

                  (if (port? file-or-port)
                      (loop-lines file-or-port)
                      (call-with-input-file file-or-port
                        loop-lines)))

                (define slurp
                  (lambda args
                    (let* ((port (apply open-pipe* OPEN_READ args))
                           (output (read-lines port))
                           (status (close-pipe port)))
                      output)))
                (let* ((response1 (slurp
                                   ,(string-append #$coreutils "/bin/cat")
                                   "/sys/fs/cgroup/cgroup.subtree_control")))
                  (sort-list (string-split (first response1) #\space) string<?)))
             marionette))

          (test-equal "/sys/fs/cgroup has correct permissions"
            '("cgroup" "cgroup")
            (marionette-eval
             `(begin
                (use-modules (ice-9 popen)
                             (ice-9 match)
                             (ice-9 rdelim))

                (define (read-lines file-or-port)
                  (define (loop-lines port)
                    (let loop ((lines '()))
                      (match (read-line port)
                        ((? eof-object?)
                         (reverse lines))
                        (line
                         (loop (cons line lines))))))

                  (if (port? file-or-port)
                      (loop-lines file-or-port)
                      (call-with-input-file file-or-port
                        loop-lines)))

                (define slurp
                  (lambda args
                    (let* ((port (apply open-pipe* OPEN_READ args))
                           (output (read-lines port))
                           (status (close-pipe port)))
                      output)))
                (let* ((bash
                        ,(string-append #$bash "/bin/bash"))
                       (response1
                        (slurp bash "-c"
                               (string-append "ls -la /sys/fs/cgroup | "
                                              "grep -E ' \\./?$' | awk '{ print $4 }'")))
                       (response2 (slurp bash "-c"
                                         (string-append "ls -l /sys/fs/cgroup/cgroup"
                                                        ".{procs,subtree_control,threads} | "
                                                        "awk '{ print $4 }' | sort -u"))))
                  (list (string-join response1 "\n") (string-join response2 "\n"))))
             marionette))

          (test-equal "Load oci image and run it (unprivileged)"
            '("hello world" "hi!" "JSON!" #o1777)
            (marionette-eval
             `(begin
                (use-modules (srfi srfi-1)
                             (ice-9 popen)
                             (ice-9 match)
                             (ice-9 rdelim))

                (define (wait-for-file file)
                  ;; Wait until FILE shows up.
                  (let loop ((i 60))
                    (cond ((file-exists? file)
                           #t)
                          ((zero? i)
                           (error "file didn't show up" file))
                          (else
                           (pk 'wait-for-file file)
                           (sleep 1)
                           (loop (- i 1))))))

                (define (read-lines file-or-port)
                  (define (loop-lines port)
                    (let loop ((lines '()))
                      (match (read-line port)
                        ((? eof-object?)
                         (reverse lines))
                        (line
                         (loop (cons line lines))))))

                  (if (port? file-or-port)
                      (loop-lines file-or-port)
                      (call-with-input-file file-or-port
                        loop-lines)))

                (define slurp
                  (lambda args
                    (let* ((port (apply open-pipe* OPEN_READ
                                        (list "sh" "-l" "-c"
                                              (string-join
                                               args
                                               " "))))
                           (output (read-lines port))
                           (status (close-pipe port)))
                      output)))

                (match (primitive-fork)
                  (0
                   (dynamic-wind
                     (const #f)
                     (lambda ()
                       (setgid (passwd:gid (getpwnam "dummy")))
                       (setuid (passwd:uid (getpw "dummy")))

                       (let* ((loaded (slurp ,(string-append #$podman
                                                             "/bin/podman")
                                             "load" "-i"
                                             ,#$oci-tarball))
                              (repository&tag "localhost/guile-guest:latest")
                              (response1 (slurp
                                          ,(string-append #$podman "/bin/podman")
                                          "run" "--pull" "never"
                                          "--entrypoint" "bin/Guile"
                                          repository&tag
                                          "/aa.scm"))
                              (response2 (slurp ;default entry point
                                          ,(string-append #$podman "/bin/podman")
                                          "run" "--pull" "never" repository&tag
                                          "-c" "'(display \"hi!\")'"))

                              ;; Check whether (json) is in $GUILE_LOAD_PATH.
                              (response3 (slurp ;default entry point + environment
                                          ,(string-append #$podman "/bin/podman")
                                          "run" "--pull" "never" repository&tag
                                          "-c" "'(use-modules (json))
  (display (json-string->scm (scm->json-string \"JSON!\")))'"))

                              ;; Check whether /tmp exists.
                              (response4 (slurp
                                          ,(string-append #$podman "/bin/podman")
                                          "run" "--pull" "never" repository&tag "-c"
                                          "'(display (stat:perms (lstat \"/tmp\")))'")))
                         (call-with-output-file (string-append ,out-dir "/response1")
                           (lambda (port)
                             (display (string-join response1 " ") port)))
                         (call-with-output-file (string-append ,out-dir "/response2")
                           (lambda (port)
                             (display (string-join response2 " ") port)))
                         (call-with-output-file (string-append ,out-dir "/response3")
                           (lambda (port)
                             (display (string-join response3 " ") port)))
                         (call-with-output-file (string-append ,out-dir "/response4")
                           (lambda (port)
                             (display (string-join response4 " ") port)))))
                     (lambda ()
                       (primitive-exit 127))))
                  (pid
                   (cdr (waitpid pid))))
                (wait-for-file (string-append ,out-dir "/response4"))
                (append
                 (slurp "cat" (string-append ,out-dir "/response1"))
                 (slurp "cat" (string-append ,out-dir "/response2"))
                 (slurp "cat" (string-append ,out-dir "/response3"))
                 (map string->number (slurp "cat" (string-append ,out-dir "/response4")))))
             marionette))

          (test-end))))

  (gexp->derivation "rootless-podman-test" test))

(define (build-tarball&run-rootless-podman-test)
  (mlet* %store-monad
      ((_ (set-grafting #f))
       (guile (set-guile-for-build (default-guile)))
       (guest-script-package ->
        (package
          (name "guest-script")
          (version "0")
          (source #f)
          (build-system trivial-build-system)
          (arguments `(#:guile ,guile-3.0
                       #:builder
                       (let ((out (assoc-ref %outputs "out")))
                         (mkdir out)
                         (call-with-output-file (string-append out "/a.scm")
                           (lambda (port)
                             (display "(display \"hello world\n\")" port)))
                         #t)))
          (synopsis "Display hello world using Guile")
          (description "This package displays the text \"hello world\" on the
standard output device and then enters a new line.")
          (home-page #f)
          (license license:public-domain)))
       (profile (profile-derivation (packages->manifest
                                     (list guile-3.0 guile-json-3
                                           guest-script-package))
                                    #:hooks '()
                                    #:locales? #f))
       (tarball (pack:docker-image
                 "docker-pack" profile
                 #:symlinks '(("/bin/Guile" -> "bin/guile")
                              ("aa.scm" -> "a.scm"))
                 #:extra-options
                 '(#:image-tag "guile-guest")
                 #:entry-point "bin/guile"
                 #:localstatedir? #t)))
    (run-rootless-podman-test tarball)))

(define %test-rootless-podman
  (system-test
   (name "rootless-podman")
   (description "Test rootless Podman service.")
   (value (build-tarball&run-rootless-podman-test))))


(define %oci-os
  (simple-operating-system
   (service dhcp-client-service-type)
   (service dbus-root-service-type)
   (service polkit-service-type)
   (service elogind-service-type)
   (service containerd-service-type)
   (service docker-service-type)
   (extra-special-file "/shared.txt"
                       (plain-file "shared.txt" "hello"))
   (service oci-container-service-type
            (list
             (oci-container-configuration
              (image
               (oci-image
                (repository "guile")
                (value
                 (specifications->manifest '("guile")))
                (pack-options
                 '(#:symlinks (("/bin" -> "bin"))))))
              (entrypoint
               "/bin/guile")
              (command
               '("-c" "(let l ((c 300))(display c)(sleep 1)(when(positive? c)(l (- c 1))))"))
              (host-environment
               '(("VARIABLE" . "value")))
              (volumes
               '(("/shared.txt" . "/shared.txt:ro")))
              (extra-arguments
               '("--env" "VARIABLE")))))))

(define (run-oci-container-test)
  "Run IMAGE as an OCI backed Shepherd service, inside OS."

  (define os
    (marionette-operating-system
     (operating-system-with-gc-roots
      %oci-os
      (list))
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (volatile? #f)
     (memory-size 1024)
     (disk-image-size (* 3000 (expt 2 20)))
     (port-forwardings '())))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-11) (srfi srfi-64)
                       (gnu build marionette))

          (define marionette
            ;; Relax timeout to accommodate older systems and
            ;; allow for pulling the image.
            (make-marionette (list #$vm) #:timeout 60))

          (test-runner-current (system-test-runner #$output))
          (test-begin "oci-container")

          (test-assert "containerd service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'containerd)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((pid) (number? pid))))))
             marionette))

          (test-assert "containerd PID file present"
            (wait-for-file "/run/containerd/containerd.pid" marionette))

          (test-assert "dockerd running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'dockerd)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((pid) (number? pid))))))
             marionette))

          (sleep 60) ; let image load

          (test-assert "docker-guile running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'docker-guile)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((pid) (number? pid))))))
             marionette))

          (test-equal "passing host environment variables and volumes"
            '("value" "hello")
            (marionette-eval
             `(begin
                (use-modules (ice-9 popen)
                             (ice-9 rdelim))

                (define slurp
                  (lambda args
                    (let* ((port (apply open-pipe* OPEN_READ args))
                           (output (let ((line (read-line port)))
                                     (if (eof-object? line)
                                         ""
                                         line)))
                           (status (close-pipe port)))
                      output)))
                (let* ((response1 (slurp
                                   ,(string-append #$docker-cli "/bin/docker")
                                   "exec" "docker-guile"
                                   "/bin/guile" "-c" "(display (getenv \"VARIABLE\"))"))
                       (response2 (slurp
                                   ,(string-append #$docker-cli "/bin/docker")
                                   "exec" "docker-guile"
                                   "/bin/guile" "-c" "(begin (use-modules (ice-9 popen) (ice-9 rdelim))
(display (call-with-input-file \"/shared.txt\" read-line)))")))
                  (list response1 response2)))
             marionette))

          (test-end))))

  (gexp->derivation "oci-container-test" test))

(define %test-oci-container
  (system-test
   (name "oci-container")
   (description "Test OCI backed Shepherd service.")
   (value (run-oci-container-test))))


(define %oci-service-os-docker
  (simple-operating-system
   (service dhcp-client-service-type)
   (service dbus-root-service-type)
   (service polkit-service-type)
   (service elogind-service-type)
   (service containerd-service-type)
   (service docker-service-type)
   (simple-service 'accounts
                   account-service-type
                   (list (user-group
                          (name "docker")
                          (system? #t))))
   (extra-special-file "/shared.txt"
                       (plain-file "shared.txt" "hello"))
   (service oci-service-type
            (oci-configuration
             (containers
              (list
               (oci-container-configuration
                (image
                 (oci-image
                  (repository "guile")
                  (value
                   (specifications->manifest '("guile")))
                  (pack-options
                   '(#:symlinks (("/bin" -> "bin"))))))
                (entrypoint
                 "/bin/guile")
                (command
                 '("-c" "(let l ((c 300))(display c)(sleep 1)(when(positive? c)(l (- c 1))))"))
                (host-environment
                 '(("VARIABLE" . "value")))
                (volumes
                 '(("/shared.txt" . "/shared.txt:ro")))
                (extra-arguments
                 '("--env" "VARIABLE")))))))))

(define %oci-service-os-podman
  (simple-operating-system
   (service dhcp-client-service-type)
   (service dbus-root-service-type)
   (service polkit-service-type)
   (service elogind-service-type)
   (service rootless-podman-service-type
            (rootless-podman-configuration
             (subgids
              (list (subid-range (name "alice"))))
             (subuids
              (list (subid-range (name "alice"))))))
   (service iptables-service-type)
   (extra-special-file "/shared.txt"
                       (plain-file "shared.txt" "hello"))
   (service oci-service-type
            (oci-configuration
             (runtime 'podman)
             (containers
              (list
               (oci-container-configuration
                (image
                 (oci-image
                  (repository "guile")
                  (value
                   (specifications->manifest '("guile")))
                  (pack-options
                   '(#:symlinks (("/bin" -> "bin"))))))
                (entrypoint
                 "/bin/guile")
                (command
                 '("-c" "(let l ((c 300))(display c)(sleep 1)(when(positive? c)(l (- c 1))))"))
                (host-environment
                 '(("VARIABLE" . "value")))
                (volumes
                 '(("/shared.txt" . "/shared.txt:ro")))
                (extra-arguments
                 '("--env" "VARIABLE")))))))))

(define (run-oci-service-test runtime oci-os)
  (define os
    (marionette-operating-system
     (operating-system-with-gc-roots
      oci-os
      (list))
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (volatile? #f)
     (memory-size 1024)
     (disk-image-size (* 3000 (expt 2 20)))
     (port-forwardings '())))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-11) (srfi srfi-64)
                       (ice-9 match)
                       (gnu build marionette))
          (define runtime-cli
            (if (eq? '#$runtime 'podman)
                "/run/current-system/profile/bin/podman"
                "/run/current-system/profile/bin/docker"))
          (define runtime-name (symbol->string '#$runtime))
          (define out-dir "/tmp")
          (define marionette
            ;; Relax timeout to accommodate older systems.
            (make-marionette (list #$vm) #:timeout 60))

          (test-runner-current (system-test-runner #$output))
          (test-begin (string-append "oci-service-" runtime-name))

          (sleep 120)                    ; let image load

          (test-assert (string-append "container running - " runtime-name)
            (marionette-eval
             `(begin
                (use-modules (gnu services herd))
                (match (start-service (symbol-append '#$runtime '-guile))
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((pid) (number? pid))))))
             marionette))

          (sleep 120)                    ; let image load

          (test-equal (string-append "passing host environment variables and volumes - " runtime-name)
            '("value" "hello")
            (marionette-eval
             `(begin
                (use-modules (srfi srfi-1)
                             (ice-9 popen)
                             (ice-9 match)
                             (ice-9 rdelim)
                             (ice-9 textual-ports))

                (define (wait-for-file file)
                  ;; Wait until FILE shows up.
                  (let loop ((i 60))
                    (cond ((file-exists? file)
                           #t)
                          ((zero? i)
                           (error "file didn't show up" file))
                          (else
                           (pk 'wait-for-file file)
                           (sleep 1)
                           (loop (- i 1))))))

                (define (read-lines file-or-port)
                  (define (loop-lines port)
                    (let loop ((lines '()))
                      (match (read-line port)
                        ((? eof-object?)
                         (reverse lines))
                        (line
                         (loop (cons line lines))))))

                  (if (port? file-or-port)
                      (loop-lines file-or-port)
                      (call-with-input-file file-or-port
                        loop-lines)))

                (define slurp
                  (lambda args
                    (let* ((port (apply open-pipe* OPEN_READ
                                        (list "sh" "-l" "-c"
                                              (string-join
                                               args
                                               " "))))
                           (output (read-lines port))
                           (status (close-pipe port)))
                      output)))

                (match (primitive-fork)
                  (0
                   (dynamic-wind
                     (const #f)
                     (lambda ()
                       (when (eq? '#$runtime 'podman)
                         (setgid (passwd:gid (getpwnam "alice")))
                         (setuid (passwd:uid (getpw "alice"))))

                       (let* ((response1 (slurp
                                          ,runtime-cli
                                          "exec" (string-append ,runtime-name "-guile")
                                          "/bin/guile" "-c" "'(display (getenv \"VARIABLE\"))'"))
                              (response2 (slurp
                                          ,runtime-cli
                                          "exec" (string-append ,runtime-name "-guile")
                                          "/bin/guile" "-c" "'(begin (use-modules (ice-9 popen) (ice-9 rdelim))
(display (call-with-input-file \"/shared.txt\" read-line)))'")))
                         (call-with-output-file (string-append ,out-dir "/response1")
                           (lambda (port)
                             (display (string-join response1 " ") port)))
                         (call-with-output-file (string-append ,out-dir "/response2")
                           (lambda (port)
                             (display (string-join response2 " ") port)))))
                     (lambda ()
                       (primitive-exit 127))))
                  (pid
                   (cdr (waitpid pid))))
                (display (call-with-input-file "/var/log/messages" get-string-all))
                (wait-for-file (string-append ,out-dir "/response2"))
                (append
                 (slurp "cat" (string-append ,out-dir "/response1"))
                 (slurp "cat" (string-append ,out-dir "/response2"))))
             marionette))

          (test-end))))

  (gexp->derivation (string-append "oci-service-test-"
                                   (symbol->string runtime))
                    test))

(define (run-oci-service-test-docker)
  (run-oci-service-test 'docker %oci-service-os-docker))

(define (run-oci-service-test-podman)
  (run-oci-service-test 'podman %oci-service-os-podman))

(define %test-oci-service-docker
  (system-test
   (name "oci-service-docker")
   (description "Test OCI provisioning Guix System service with Docker backend.")
   (value (run-oci-service-test-docker))))

(define %test-oci-service-podman
  (system-test
   (name "oci-service-podman")
   (description "Test OCI provisioning Guix System service with Podman backend.")
   (value (run-oci-service-test-podman))))
