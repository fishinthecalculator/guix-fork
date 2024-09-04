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

(define-module (gnu services containers)
  #:use-module (gnu image)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages containers)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu system image)
  #:use-module (gnu system shadow)
  #:use-module (gnu system pam)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module ((guix scripts pack) #:prefix pack:)
  #:use-module (guix store)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (rootless-podman-configuration
            rootless-podman-configuration?
            rootless-podman-configuration-fields
            rootless-podman-configuration-podman
            rootless-podman-configuration-group-name
            rootless-podman-configuration-docker-emulation?
            rootless-podman-configuration-containers-registries
            rootless-podman-configuration-containers-storage
            rootless-podman-configuration-containers-policy
            rootless-podman-configuration-pam-limits
            rootless-podman-configuration-subgids
            rootless-podman-configuration-subuids

            rootless-podman-service-subids
            rootless-podman-service-accounts
            rootless-podman-service-profile
            rootless-podman-shepherd-services
            rootless-podman-service-etc

            rootless-podman-service-type

            oci-image
            oci-image?
            oci-image-fields
            oci-image-repository
            oci-image-tag
            oci-image-value
            oci-image-pack-options
            oci-image-target
            oci-image-system
            oci-image-grafts?

            oci-container-configuration
            oci-container-configuration?
            oci-container-configuration-fields
            oci-container-configuration-user
            oci-container-configuration-group
            oci-container-configuration-command
            oci-container-configuration-entrypoint
            oci-container-configuration-host-environment
            oci-container-configuration-environment
            oci-container-configuration-image
            oci-container-configuration-provision
            oci-container-configuration-requirement
            oci-container-configuration-log-file
            oci-container-configuration-auto-start?
            oci-container-configuration-respawn?
            oci-container-configuration-shepherd-actions
            oci-container-configuration-network
            oci-container-configuration-ports
            oci-container-configuration-volumes
            oci-container-configuration-container-user
            oci-container-configuration-workdir
            oci-container-configuration-extra-arguments

            oci-network-configuration
            oci-network-configuration?
            oci-network-configuration-fields
            oci-network-configuration-name
            oci-network-configuration-driver
            oci-network-configuration-gateway
            oci-network-configuration-internal?
            oci-network-configuration-ip-range
            oci-network-configuration-ipam-driver
            oci-network-configuration-ipv6?
            oci-network-configuration-subnet
            oci-network-configuration-labels
            oci-network-configuration-extra-arguments

            oci-volume-configuration
            oci-volume-configuration?
            oci-volume-configuration-fields
            oci-volume-configuration-name
            oci-volume-configuration-labels
            oci-volume-configuration-extra-arguments

            oci-configuration
            oci-configuration?
            oci-configuration-fields
            oci-configuration-runtime
            oci-configuration-user
            oci-configuration-group
            oci-configuration-containers
            oci-configuration-networks
            oci-configuration-volumes

            oci-extension
            oci-extension?
            oci-extension-fields
            oci-extension-containers
            oci-extension-networks
            oci-extension-volumes

            oci-container-service-type
            oci-service-type
            oci-service-accounts
            oci-service-subids
            oci-configuration->shepherd-services))

(define (gexp-or-string? value)
  (or (gexp? value)
      (string? value)))

(define (lowerable? value)
  (or (file-like? value)
      (gexp-or-string? value)))

(define list-of-pam-limits-entries?
  (list-of pam-limits-entry?))

(define list-of-subid-ranges?
  (list-of subid-range?))

(define-configuration/no-serialization rootless-podman-configuration
  (podman
   (package podman)
   "The Podman package that will be installed in the system profile.")
  (group-name
   (string "cgroup")
   "The name of the group that will own /sys/fs/cgroup.  Users that want to use
rootless podman have to be in this group.")
  (docker-emulation?
   (boolean #f)
   "When true the service will install a symlink to the system profile allowing
Docker commands to be run through Podman.")
  (containers-registries
   (lowerable
    (plain-file "registries.conf"
                (string-append "unqualified-search-registries = ['docker.io','"
                               "registry.fedora.org','registry.opensuse.org']")))
   "A string or a gexp evaluating to the path of Podman's
@code{containers/registries.conf} configuration file.")
  (containers-storage
   (lowerable
    (plain-file "storage.conf"
                "[storage]
driver = \"overlay\""))
   "A string or a gexp evaluating to the path of Podman's
@code{containers/storage.conf} configuration file.")
  (containers-policy
   (lowerable
    (plain-file "policy.json"
                "{\"default\": [{\"type\": \"insecureAcceptAnything\"}]}"))
   "A string or a gexp evaluating to the path of Podman's
@code{containers/policy.json} configuration file.")
  (pam-limits
   (list-of-pam-limits-entries
    (list (pam-limits-entry "*" 'both 'nofile 100000)))
   "The PAM limits to be set for rootless Podman.")
  (subgids
   (list-of-subid-ranges '())
   "A list of subid ranges representing the subgids that will be
available for each configured user.")
  (subuids
   (list-of-subid-ranges '())
   "A list of subid ranges representing the subuids that will be
available for each configured user."))

(define-public (rootless-podman-docker-emulation podman)
  (package
    (inherit docker-cli)
    (name "rootless-podman-docker-emulation")
    (version "0.0.0")
    (source podman)
    (build-system trivial-build-system)
    (arguments
     (list #:modules '((guix build utils))
           #:builder
           #~(begin
               (use-modules (guix build utils))
               (let* ((bin (string-append #$output "/bin"))
                      (target (string-append bin "/docker")))
                 (mkdir-p bin)
                 (symlink
                  (string-append #$source "/bin/podman")
                  target)))))
    (synopsis "Rootless Podman backed Docker emulation")
    (description "This package provides a symlink enabling to run Docker
commands through Podman.")
    (license license:gpl3+)))

(define (rootless-podman-service-profile config)
  (let ((podman
         (rootless-podman-configuration-podman config)))
    (append
     (if (rootless-podman-configuration-docker-emulation? config)
         (list (rootless-podman-docker-emulation podman))
         '())
     (list podman))))

(define rootless-podman-service-etc
  (lambda (config)
    (list `("containers/registries.conf"
            ,(rootless-podman-configuration-containers-registries config))
          `("containers/storage.conf"
            ,(rootless-podman-configuration-containers-storage config))
          `("containers/policy.json"
            ,(rootless-podman-configuration-containers-policy config)))))

(define rootless-podman-service-subids
  (lambda (config)
    (subids-extension
     (subgids (rootless-podman-configuration-subgids config))
     (subuids (rootless-podman-configuration-subuids config)))))

(define rootless-podman-service-accounts
  (lambda (config)
    (list (user-group (name (rootless-podman-configuration-group-name config))
                      (system? #t)))))

(define (cgroups-fs-owner-entrypoint config)
  (define group
    (rootless-podman-configuration-group-name config))
  (program-file "cgroups2-fs-owner-entrypoint"
                #~(system*
                   "bash" "-c"
                   (string-append "echo Setting /sys/fs/cgroup "
                                  "group ownership to " #$group " && chown -v "
                                  "root:" #$group " /sys/fs/cgroup && "
                                  "chmod -v 775 /sys/fs/cgroup && chown -v "
                                  "root:" #$group " /sys/fs/cgroup/cgroup."
                                  "{procs,subtree_control,threads} && "
                                  "chmod -v 664 /sys/fs/cgroup/cgroup."
                                  "{procs,subtree_control,threads}"))))

(define (rootless-podman-cgroups-fs-owner-service config)
  (shepherd-service (provision '(cgroups2-fs-owner))
                    (requirement
                     '(dbus-system
                       elogind
                       file-system-/sys/fs/cgroup
                       networking
                       udev
                       cgroups2-limits))
                    (one-shot? #t)
                    (documentation
                     "Set ownership of /sys/fs/cgroup to the configured group.")
                    (start
                     #~(make-forkexec-constructor
                        (list
                         #$(cgroups-fs-owner-entrypoint config))))
                    (stop
                     #~(make-kill-destructor))))

(define cgroups-limits-entrypoint
  (program-file "cgroups2-limits-entrypoint"
                #~(system*
                   "bash" "-c"
                   (string-append "echo Setting cgroups v2 limits && "
                                  "echo +cpu +cpuset +io +memory +pids"
                                  " >> /sys/fs/cgroup/cgroup.subtree_control"))))

(define (rootless-podman-cgroups-limits-service config)
  (shepherd-service (provision '(cgroups2-limits))
                    (requirement
                     '(dbus-system
                       elogind
                       networking
                       udev
                       file-system-/sys/fs/cgroup
                       rootless-podman-shared-root-fs))
                    (one-shot? #t)
                    (documentation
                     "Allow setting cgroups limits: cpu, cpuset, memory and
pids.")
                    (start
                     #~(make-forkexec-constructor
                        (list
                         #$cgroups-limits-entrypoint)))
                    (stop
                     #~(make-kill-destructor))))

(define rootless-podman-shared-root-fs-entrypoint
  (program-file "rootless-podman-shared-root-fs-entrypoint"
                #~(system*
                   "mount" "--make-shared" "/")))

(define (rootless-podman-shared-root-fs-service config)
  (shepherd-service (provision '(rootless-podman-shared-root-fs))
                    (requirement
                     '(user-processes))
                    (one-shot? #t)
                    (documentation
                     "Buildah/Podman running as rootless expects the bind mount
to be shared.  This service sets it so.")
                    (start
                     #~(make-forkexec-constructor
                        (list
                         #$rootless-podman-shared-root-fs-entrypoint)))
                    (stop
                     #~(make-kill-destructor))))

(define (rootless-podman-shepherd-services config)
  (list
   (rootless-podman-shared-root-fs-service config)
   (rootless-podman-cgroups-limits-service config)
   (rootless-podman-cgroups-fs-owner-service config)))

(define rootless-podman-service-type
  (service-type (name 'rootless-podman)
                (extensions
                 (list
                  (service-extension subids-service-type
                                     rootless-podman-service-subids)
                  (service-extension account-service-type
                                     rootless-podman-service-accounts)
                  (service-extension profile-service-type
                                     rootless-podman-service-profile)
                  (service-extension shepherd-root-service-type
                                     rootless-podman-shepherd-services)
                  (service-extension pam-limits-service-type
                                     rootless-podman-configuration-pam-limits)
                  (service-extension etc-service-type
                                     rootless-podman-service-etc)))
                (default-value (rootless-podman-configuration))
                (description
                 "This service configures rootless @code{podman} on the Guix System.")))


;;;
;;; OCI container.
;;;

(define %oci-supported-runtimes
  '(docker podman))

(define (oci-runtime-requirement runtime)
  (if (eq? 'podman runtime)
      '(cgroups2-fs-owner cgroups2-limits
        rootless-podman-shared-root-fs)
      '(dockerd)))

(define* (oci-runtime-cli runtime #:key (system? #t))
  (if (eq? 'podman runtime)
      (if system?
          ;; Use binaries from PATH to allow
          ;; users to override them with their
          ;; respective Guix System services.
          "podman"
          (file-append podman "/bin/podman"))
      (if system?
          "docker"
          (file-append docker-cli "/bin/docker"))))

(define (oci-runtime-name runtime)
  (if (eq? 'podman runtime)
      "Podman" "Docker"))

(define (oci-runtime-group runtime maybe-group)
  (if (maybe-value-set? maybe-group)
      (if (eq? 'podman runtime)
          "cgroup"
          "docker")))

(define (oci-sanitize-runtime value)
  (unless (member value %oci-supported-runtimes)
    (raise
     (formatted-message
      (G_ "OCI runtime must be a symbol and one of ~a,
but ~a was found") %oci-supported-runtimes value))))

(define (oci-sanitize-pair pair delimiter)
  (define (valid? member)
    (or (string? member)
        (gexp? member)
        (file-like? member)))
  (match pair
    (((? valid? key) . (? valid? value))
     #~(string-append #$key #$delimiter #$value))
    (_
     (raise
      (formatted-message
       (G_ "pair members must contain only strings, gexps or file-like objects
but ~a was found")
       pair)))))

(define (oci-sanitize-mixed-list name value delimiter)
  (map
   (lambda (el)
     (cond ((string? el) el)
           ((pair? el) (oci-sanitize-pair el delimiter))
           (else
            (raise
             (formatted-message
              (G_ "~a members must be either a string or a pair but ~a was
found!")
              name el)))))
   value))

(define (oci-sanitize-host-environment value)
  ;; Expected spec format:
  ;; '(("HOME" . "/home/nobody") "JAVA_HOME=/java")
  (oci-sanitize-mixed-list "host-environment" value "="))

(define (oci-sanitize-environment value)
  ;; Expected spec format:
  ;; '(("HOME" . "/home/nobody") "JAVA_HOME=/java")
  (oci-sanitize-mixed-list "environment" value "="))

(define (oci-sanitize-ports value)
  ;; Expected spec format:
  ;; '(("8088" . "80") "2022:22")
  (oci-sanitize-mixed-list "ports" value ":"))

(define (oci-sanitize-volumes value)
  ;; Expected spec format:
  ;; '(("/mnt/dir" . "/dir") "/run/current-system/profile:/java")
  (oci-sanitize-mixed-list "volumes" value ":"))

(define (oci-sanitize-labels value)
  ;; Expected spec format:
  ;; '(("foo" . "bar") "foo=bar")
  (oci-sanitize-mixed-list "labels" value "="))

(define (oci-sanitize-shepherd-actions value)
  (map
   (lambda (el)
     (if (shepherd-action? el)
         el
         (raise
          (formatted-message
           (G_ "shepherd-actions may only be shepherd-action records
but ~a was found") el))))
   value))

(define (oci-sanitize-extra-arguments value)
  (define (valid? member)
    (or (string? member)
        (gexp? member)
        (file-like? member)))
  (map
   (lambda (el)
     (if (valid? el)
         el
         (raise
          (formatted-message
           (G_ "extra arguments may only be strings, gexps or file-like objects
but ~a was found") el))))
   value))

(define (oci-image-reference image)
  (if (string? image)
      image
      (string-append (oci-image-repository image)
                     ":" (oci-image-tag image))))

(define (oci-lowerable-image? image)
  (or (manifest? image)
      (operating-system? image)
      (gexp? image)
      (file-like? image)))

(define (string-or-oci-image? image)
  (or (string? image)
      (oci-image? image)))

(define list-of-symbols?
  (list-of symbol?))

(define (list-of-oci-records? name predicate value)
  (map
   (lambda (el)
     (if (predicate el)
         el
         (raise
          (formatted-message
           (G_ "~a contains an illegal value: ~a") name el))))
   value))

(define (list-of-oci-containers? value)
  (list-of-oci-records? "containers" oci-container-configuration? value))
(define (list-of-oci-networks? value)
  (list-of-oci-records? "networks" oci-network-configuration? value))
(define (list-of-oci-volumes? value)
  (list-of-oci-records? "volumes" oci-volume-configuration? value))

(define-maybe/no-serialization string)

(define-configuration/no-serialization oci-image
  (repository
   (string)
   "A string like @code{myregistry.local:5000/testing/test-image} that names
the OCI image.")
  (tag
   (string "latest")
   "A string representing the OCI image tag. Defaults to @code{latest}.")
  (value
   (oci-lowerable-image)
   "A @code{manifest} or @code{operating-system} record that will be lowered
into an OCI compatible tarball.  Otherwise this field's value can be a gexp
or a file-like object that evaluates to an OCI compatible tarball.")
  (pack-options
   (list '())
   "An optional set of keyword arguments that will be passed to the
@code{docker-image} procedure from @code{guix scripts pack}.  They can be used
to replicate @command{guix pack} behavior:

@lisp
(oci-image
  (repository \"guile\")
  (tag \"3\")
  (manifest (specifications->manifest '(\"guile\")))
  (pack-options
    '(#:symlinks ((\"/bin/guile\" -> \"bin/guile\"))
      #:max-layers 2)))
@end lisp

If the @code{value} field is an @code{operating-system} record, this field's
value will be ignored.")
  (system
   (maybe-string)
   "Attempt to build for a given system, e.g. \"i686-linux\"")
  (target
   (maybe-string)
   "Attempt to cross-build for a given triple, e.g. \"aarch64-linux-gnu\"")
  (grafts?
   (boolean #f)
   "Whether to allow grafting or not in the pack build."))

(define-configuration/no-serialization oci-container-configuration
  (user
   (string "oci-container")
   "The user under whose authority docker commands will be run.")
  (group
   (string "docker")
   "The group under whose authority docker commands will be run.")
  (command
   (list-of-strings '())
   "Overwrite the default command (@code{CMD}) of the image.")
  (entrypoint
   (maybe-string)
   "Overwrite the default entrypoint (@code{ENTRYPOINT}) of the image.")
  (host-environment
   (list '())
   "Set environment variables in the host environment where @command{docker run}
is invoked.  This is especially useful to pass secrets from the host to the
container without having them on the @command{docker run}'s command line: by
setting the @code{MYSQL_PASSWORD} on the host and by passing
@code{--env MYSQL_PASSWORD} through the @code{extra-arguments} field, it is
possible to securely set values in the container environment.  This field's
value can be a list of pairs or strings, even mixed:

@lisp
(list '(\"LANGUAGE\" . \"eo:ca:eu\")
      \"JAVA_HOME=/opt/java\")
@end lisp

Pair members can be strings, gexps or file-like objects. Strings are passed
directly to @code{make-forkexec-constructor}."
   (sanitizer oci-sanitize-host-environment))
  (environment
   (list '())
   "Set environment variables inside the container.  This can be a list of pairs
or strings, even mixed:

@lisp
(list '(\"LANGUAGE\" . \"eo:ca:eu\")
      \"JAVA_HOME=/opt/java\")
@end lisp

Pair members can be strings, gexps or file-like objects. Strings are passed
directly to the Docker CLI.  You can refer to the
@url{https://docs.docker.com/engine/reference/commandline/run/#env,upstream}
documentation for semantics."
   (sanitizer oci-sanitize-environment))
  (image
   (string-or-oci-image)
   "The image used to build the container.  It can be a string or an
@code{oci-image} record.  Strings are resolved by the Docker
Engine, and follow the usual format
@code{myregistry.local:5000/testing/test-image:tag}.")
  (provision
   (maybe-string)
   "Set the name of the provisioned Shepherd service.")
  (requirement
   (list-of-symbols '())
   "Set additional Shepherd services dependencies to the provisioned Shepherd
service.")
  (log-file
   (maybe-string)
   "When @code{log-file} is set, it names the file to which the service’s
standard output and standard error are redirected.  @code{log-file} is created
if it does not exist, otherwise it is appended to.")
  (auto-start?
   (boolean #t)
   "Whether this service should be started automatically by the Shepherd.  If it
is @code{#f} the service has to be started manually with @command{herd start}.")
  (respawn?
   (boolean #f)
   "Whether to restart the service when it stops, for instance when the
underlying process dies.")
  (shepherd-actions
   (list '())
   "This is a list of @code{shepherd-action} records defining actions supported
by the service."
   (sanitizer oci-sanitize-shepherd-actions))
  (network
   (maybe-string)
   "Set a Docker network for the spawned container.")
  (ports
   (list '())
   "Set the port or port ranges to expose from the spawned container.  This can
be a list of pairs or strings, even mixed:

@lisp
(list '(\"8080\" . \"80\")
      \"10443:443\")
@end lisp

Pair members can be strings, gexps or file-like objects. Strings are passed
directly to the Docker CLI.  You can refer to the
@url{https://docs.docker.com/engine/reference/commandline/run/#publish,upstream}
documentation for semantics."
   (sanitizer oci-sanitize-ports))
  (volumes
   (list '())
   "Set volume mappings for the spawned container.  This can be a
list of pairs or strings, even mixed:

@lisp
(list '(\"/root/data/grafana\" . \"/var/lib/grafana\")
      \"/gnu/store:/gnu/store\")
@end lisp

Pair members can be strings, gexps or file-like objects. Strings are passed
directly to the Docker CLI.  You can refer to the
@url{https://docs.docker.com/engine/reference/commandline/run/#volume,upstream}
documentation for semantics."
   (sanitizer oci-sanitize-volumes))
  (container-user
   (maybe-string)
   "Set the current user inside the spawned container.  You can refer to the
@url{https://docs.docker.com/engine/reference/run/#user,upstream}
documentation for semantics.")
  (workdir
   (maybe-string)
   "Set the current working for the spawned Shepherd service.
You can refer to the
@url{https://docs.docker.com/engine/reference/run/#workdir,upstream}
documentation for semantics.")
  (extra-arguments
   (list '())
   "A list of strings, gexps or file-like objects that will be directly passed
to the @command{docker run} invokation."
   (sanitizer oci-sanitize-extra-arguments)))

(define-configuration/no-serialization oci-volume-configuration
  (name
   (string)
   "The name of the OCI volume to provision.")
  (labels
   (list '())
   "The list of labels that will be used to tag the current volume."
   (sanitizer oci-sanitize-labels))
  (extra-arguments
   (list '())
   "A list of strings, gexps or file-like objects that will be directly passed
to the runtime invokation."
   (sanitizer oci-sanitize-extra-arguments)))

(define-configuration/no-serialization oci-network-configuration
  (name
   (string)
   "The name of the OCI network to provision.")
  (driver
   (string "bridge")
   "The driver to manage the network.")
  (gateway
   (maybe-string)
   "IPv4 or IPv6 gateway for the subnet.")
  (internal?
   (boolean #f)
   "Restrict external access to the network")
  (ip-range
   (maybe-string)
   "Allocate container ip from a sub-range in CIDR format.")
  (ipam-driver
   (string "default")
   "IP Address Management Driver.")
  (ipv6?
   (boolean #f)
   "Enable IPv6 networking.")
  (subnet
   (maybe-string)
   "Subnet in CIDR format that represents a network segment.")
  (labels
   (list '())
   "The list of labels that will be used to tag the current volume."
   (sanitizer oci-sanitize-labels))
  (extra-arguments
   (list '())
   "A list of strings, gexps or file-like objects that will be directly passed
to the runtime invokation."
   (sanitizer oci-sanitize-extra-arguments)))

(define-configuration/no-serialization oci-configuration
  (runtime
   (symbol 'docker)
   "The OCI runtime to use to run commands."
   (sanitizer oci-sanitize-runtime))
  (user
   (string "oci-container")
   "The user under whose authority OCI runtime commands will be run.")
  (group
   (maybe-string)
   "The group under whose authority OCI commands will be run.  Its default value
is either @code{docker} or @code{cgroups} based on the selected OCI runtime.")
  (containers
   (list-of-oci-containers '())
   "The list of @code{oci-container-configuration} records representing the
containers to provision.")
  (networks
   (list-of-oci-networks '())
   "The list of @code{oci-network-configuration} records representing the
networks to provision.")
  (volumes
   (list-of-oci-volumes '())
   "The list of @code{oci-volume-configuration} records representing the
volumes to provision."))

(define-configuration/no-serialization oci-extension
  (containers
   (list-of-oci-containers '())
   "The list of @code{oci-container-configuration} records representing the
containers to add.")
  (networks
   (list-of-oci-networks '())
   "The list of @code{oci-network-configuration} records representing the
networks to add.")
  (volumes
   (list-of-oci-volumes '())
   "The list of @code{oci-volume-configuration} records representing the
volumes to add."))

(define (oci-image->container-name image)
  (basename
   (if (string? image)
       (first (string-split image #\:))
       (oci-image-repository image))))

(define (oci-container-shepherd-name runtime config)
  (define name (symbol->string
                (oci-container-configuration-provision config)))
  (define image (oci-container-configuration-image config))
  (string->symbol
   (if (maybe-value-set? name)
       name
       (string-append (symbol->string runtime) "-"
                      (oci-image->container-name image)))))

(define (oci-network-shepherd-name runtime)
 (string->symbol
  (string-append (symbol->string runtime) "-networks")))

(define (oci-volume-shepherd-name runtime)
  (string->symbol
   (string-append (symbol->string runtime) "-volumes")))

(define oci-container-configuration->options
  (lambda (config)
    (let ((entrypoint
           (oci-container-configuration-entrypoint config))
          (network
           (oci-container-configuration-network config))
          (user
           (oci-container-configuration-container-user config))
          (workdir
           (oci-container-configuration-workdir config)))
      (apply append
             (filter (compose not unspecified?)
                     `(,(if (maybe-value-set? entrypoint)
                            `("--entrypoint" ,entrypoint)
                            '())
                       ,(append-map
                         (lambda (spec)
                           (list "--env" spec))
                         (oci-container-configuration-environment config))
                       ,(if (maybe-value-set? network)
                            `("--network" ,network)
                            '())
                       ,(if (maybe-value-set? user)
                            `("--user" ,user)
                            '())
                       ,(if (maybe-value-set? workdir)
                            `("--workdir" ,workdir)
                            '())
                       ,(append-map
                         (lambda (spec)
                           (list "-p" spec))
                         (oci-container-configuration-ports config))
                       ,(append-map
                         (lambda (spec)
                           (list "-v" spec))
                         (oci-container-configuration-volumes config))))))))

(define (oci-network-configuration->options config)
  (let ((driver (oci-network-configuration-driver config))
        (gateway
         (oci-network-configuration-driver config))
        (internal?
         (oci-network-configuration-internal? config))
        (ip-range
         (oci-network-configuration-ip-range config))
        (ipam-driver
         (oci-network-configuration-ipam-driver config))
        (ipv6?
         (oci-network-configuration-ipv6? config))
        (subnet
         (oci-network-configuration-subnet config)))
    (apply append
           (filter (compose not unspecified?)
                   `(,(if (maybe-value-set? gateway)
                          `("--gateway" ,gateway)
                          '())
                     ,(if internal?
                          `("--internal")
                          '())
                     ,(if (maybe-value-set? ip-range)
                          `("--ip-range" ,ip-range)
                          '())
                     "--ipam-driver" ,ipam-driver
                     ,(if ipv6?
                          `("--ipv6")
                          '())
                     ,(if (maybe-value-set? subnet)
                          `("--subnet" ,subnet)
                          '())
                     ,(append-map
                       (lambda (spec)
                         (list "--label" spec))
                       (oci-network-configuration-labels config)))))))

(define (oci-volume-configuration->options config)
  (append-map
   (lambda (spec)
     (list "--label" spec))
   (oci-volume-configuration-labels config)))

(define* (get-keyword-value args keyword #:key (default #f))
  (let ((kv (memq keyword args)))
    (if (and kv (>= (length kv) 2))
        (cadr kv)
        default)))

(define (lower-operating-system os target system)
  (mlet* %store-monad
      ((tarball
        (lower-object
         (system-image (os->image os #:type docker-image-type))
         system
         #:target target)))
    (return tarball)))

(define (lower-manifest name image target system)
  (define value (oci-image-value image))
  (define options (oci-image-pack-options image))
  (define image-reference
    (oci-image-reference image))
  (define image-tag
    (let* ((extra-options
            (get-keyword-value options #:extra-options))
           (image-tag-option
            (and extra-options
                 (get-keyword-value extra-options #:image-tag))))
      (if image-tag-option
          '()
          `(#:extra-options (#:image-tag ,image-reference)))))

  (mlet* %store-monad
      ((_ (set-grafting
           (oci-image-grafts? image)))
       (guile (set-guile-for-build (default-guile)))
       (profile
        (profile-derivation value
                            #:target target
                            #:system system
                            #:hooks '()
                            #:locales? #f))
       (tarball (apply pack:docker-image
                       `(,name ,profile
                         ,@options
                         ,@image-tag
                         #:localstatedir? #t))))
    (return tarball)))

(define (lower-oci-image name image)
  (define value (oci-image-value image))
  (define image-target (oci-image-target image))
  (define image-system (oci-image-system image))
  (define target
    (if (maybe-value-set? image-target)
        image-target
        (%current-target-system)))
  (define system
    (if (maybe-value-set? image-system)
        image-system
        (%current-system)))
  (with-store store
   (run-with-store store
     (match value
       ((? manifest? value)
        (lower-manifest name image target system))
       ((? operating-system? value)
        (lower-operating-system value target system))
       ((or (? gexp? value)
            (? file-like? value))
        value)
       (_
        (raise
         (formatted-message
          (G_ "oci-image value must contain only manifest,
operating-system, gexp or file-like records but ~a was found")
          value))))
     #:target target
     #:system system)))

(define (oci-image-loader runtime name image tag)
  (let ((tarball (lower-oci-image name image)))
    (with-imported-modules '((guix build utils))
      (program-file (format #f "~a-image-loader" name)
       #~(begin
           (use-modules (guix build utils)
                        (ice-9 popen)
                        (ice-9 rdelim))

           (format #t "Loading image for ~a from ~a...~%" #$name #$tarball)
           (define line
             (read-line
              (open-input-pipe
               (string-append #$(oci-runtime-cli runtime)
                              " load -i " #$tarball))))

           (unless (or (eof-object? line)
                       (string-null? line))
             (format #t "~a~%" line)
             (let ((repository&tag
                    (string-drop line
                                 (string-length
                                   "Loaded image: "))))

               (invoke #$(oci-runtime-cli runtime) "tag" repository&tag #$tag)
               (format #t "Tagged ~a with ~a...~%" #$tarball #$tag))))))))

(define* (oci-container-shepherd-service runtime config #:key (user #f) (group #f))
  (let* ((actions (oci-container-configuration-shepherd-actions config))
         (auto-start?
          (oci-container-configuration-auto-start? config))
         (user (or user (oci-container-configuration-user config)))
         (group (if (and group (maybe-value-set? group))
                    group
                    (oci-container-configuration-group config)))
         (host-environment
          (oci-container-configuration-host-environment config))
         (command (oci-container-configuration-command config))
         (log-file (oci-container-configuration-log-file config))
         (provision (oci-container-configuration-provision config))
         (requirement (oci-container-configuration-requirement config))
         (respawn?
          (oci-container-configuration-respawn? config))
         (image (oci-container-configuration-image config))
         (image-reference (oci-image-reference image))
         (options (oci-container-configuration->options config))
         (name
          (oci-container-shepherd-name runtime config))
         (extra-arguments
          (oci-container-configuration-extra-arguments config)))

    (shepherd-service (provision `(,(string->symbol name)))
                      (requirement `(,@(oci-runtime-requirement runtime)
                                     user-processes
                                     ,(oci-network-shepherd-name runtime)
                                     ,(oci-volume-shepherd-name runtime)
                                     ,@requirement))
                      (respawn? respawn?)
                      (auto-start? auto-start?)
                      (documentation
                       (string-append
                        (oci-runtime-name runtime) " backed Shepherd service for "
                        (if (oci-image? image) name image) "."))
                      (start
                       #~(lambda ()
                           #$@(if (oci-image? image)
                                  #~((invoke #$(oci-image-loader runtime
                                                name image image-reference)))
                                  #~())
                           (fork+exec-command
                            ;; run [OPTIONS] IMAGE [COMMAND] [ARG...]
                            (list #$(oci-runtime-cli runtime) "run" "--rm" "--name" #$name
                                  #$@options #$@extra-arguments
                                  #$image-reference #$@command)
                            #:user #$user
                            #:group #$group
                            #$@(if (maybe-value-set? log-file)
                                   (list #:log-file log-file)
                                   '())
                            #:environment-variables
                            (list #$@host-environment))))
                      (stop
                       #~(lambda _
                           (invoke #$(oci-runtime-cli runtime) "rm" "-f" #$name)))
                      (actions
                       (if (oci-image? image)
                           '()
                           (append
                            (list
                             (shepherd-action
                              (name 'pull)
                              (documentation
                               (format #f "Pull ~a's image (~a)."
                                       name image))
                              (procedure
                               #~(lambda _
                                   (invoke #$(oci-runtime-cli runtime) "pull" #$image)))))
                            actions))))))

(define (oci-object-create-script object runtime objects-sexps)
  (define runtime-string (symbol->string runtime))
  (program-file
   (string-append runtime-string "-" object "-"
                  (oci-runtime-name runtime) "-create.scm")
   #~(begin
       (use-modules (ice-9 match)
                    (ice-9 popen)
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

       (define (network-exists? name)
         (if (string=? #$runtime-string "podman")
             (equal? EXIT_SUCCESS
                     (system* #$(oci-runtime-cli runtime)
                              #$object "exists" name))
             (member name
                     (read-lines
                      (open-input-pipe
                       (string-append #$(oci-runtime-cli runtime)
                                      " " object " ls --format "
                                      "\"{{.Name}}\""))))))

       (for-each
        (match-lambda
          ((name options extra-arguments)
           (if (network-exists? name)
               (display (string-append #$(oci-runtime-name runtime)
                                       " " name " " object " already exists,"
                                       " skipping creation."))
               ;; network|volume create [options] [NAME]
               (apply system `(#$(oci-runtime-cli runtime) object "create"
                               ,@options ,@extra-arguments ,name)))))
        '#$objects-sexps))))


(define (oci-network-create-script runtime networks)
 (oci-object-create-script "network" runtime
                           (map (lambda (n) (list (oci-network-configuration-name n)
                                                  (oci-network-configuration->options n)
                                                  (oci-network-configuration-extra-arguments n)))
                                networks)))

(define (oci-volume-create-script runtime volumes)
  (oci-object-create-script "volume" runtime
                            (map (lambda (n) (list (oci-volume-configuration-name n)
                                                   (oci-volume-configuration->options n)
                                                   (oci-volume-configuration-extra-arguments n)))
                                 volumes)))

(define* (oci-network-shepherd-service config
                                       #:key (user #f)
                                             (group #f))
  (let* ((runtime (oci-configuration-runtime config))
         (requirement
          (oci-runtime-requirement runtime))
         (networks
          (oci-configuration-networks config))
         (name (oci-network-shepherd-name runtime)))

    (shepherd-service (provision `(,(string->symbol name)))
                      (requirement `(user-processes ,@requirement))
                      (one-shot? #t)
                      (documentation
                       (string-append
                        (oci-runtime-name runtime)
                        " network provisioning service"))
                      (start
                       #~((fork+exec-command
                            (list
                             #$(oci-network-create-script runtime
                                                          networks))
                            #:user #$user
                            #:group #$group))))))

(define* (oci-volume-shepherd-service config #:key (user #f) (group #f))
  (let* ((runtime (oci-configuration-runtime config))
         (requirement
          (oci-runtime-requirement runtime))
         (volumes
          (oci-configuration-volumes config))
         (name (oci-volume-shepherd-name runtime)))

    (shepherd-service (provision `(,(string->symbol name)))
                      (requirement `(user-processes ,@requirement))
                      (one-shot? #t)
                      (documentation
                       (string-append
                        (oci-runtime-name runtime)
                        " volume provisioning service"))
                      (start
                       #~((fork+exec-command
                           (list
                            #$(oci-volume-create-script runtime
                                                        volumes))
                           #:user #$user
                           #:group #$group))))))

(define (oci-service-accounts config)
  (define user (oci-configuration-user config))
  (define maybe-group (oci-configuration-group config))
  (define runtime (oci-configuration-runtime config))
  (list (user-account
         (name user)
         (comment "OCI services account")
         (group (oci-runtime-group runtime maybe-group))
         (system? #t)
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define (oci-configuration->shepherd-services config)
  (let* ((runtime (oci-configuration-runtime config))
         (user (oci-container-configuration-user config))
         (maybe-group (oci-container-configuration-group config))
         (group (oci-runtime-group runtime maybe-group)))
    (append (map (lambda (c)
                   (oci-container-shepherd-service runtime c
                                                   #:user user
                                                   #:group group))
                 (oci-configuration-containers config))
            (list
             (oci-network-shepherd-service config #:user user #:group group)
             (oci-volume-shepherd-service config #:user user #:group group)))))

(define (oci-service-subids c)
  (define runtime
    (oci-configuration-runtime c))
  (define user
    (oci-configuration-user c))
  (define container-users
    (map (lambda (name) (subid-range (name name)))
         (append (list user)
                 (map (lambda (cc)
                        (oci-container-configuration-user cc))
                      (oci-configuration-containers c)))))
  (if (eq? 'podman runtime)
      (subids-extension
       (subgids
        container-users)
       (subuids
        container-users))
      (subids-extension)))

(define (oci-objects-merge-lst a b object get-name)
  (define (contains? value lst)
    (member value (map get-name lst)))
  (let loop ((merged '())
             (lst (append a b)))
    (if (null? lst)
        merged
        (loop
         (let ((element (car lst)))
           (when (contains? element merged)
             (raise
              (formatted-message
               (G_ "Duplicated ~a: ~a. ~as names should be unique, please
remove the duplicate.") object (get-name element) object))))
         (cdr lst)))))

(define (oci-extension-merge a b)
  (oci-extension
   (containers (oci-objects-merge-lst
                (oci-extension-containers a)
                (oci-extension-containers b)
                "container"
                (lambda (config)
                  (define maybe-name (oci-container-configuration-provision config))
                  (if (maybe-value-set? maybe-name)
                      (symbol->string maybe-name)
                      (oci-image->container-name
                       (oci-container-configuration-image config))))))
   (networks (oci-objects-merge-lst
              (oci-extension-networks a)
              (oci-extension-networks b)
              "network"
              oci-network-shepherd-name))
   (volumes (oci-objects-merge-lst
             (oci-extension-volumes a)
             (oci-extension-volumes b)
             "volume"
             oci-volume-shepherd-name))))

(define oci-service-type
  (service-type (name 'oci)
                (extensions
                 (list (service-extension profile-service-type
                                          (lambda (config)
                                            (list bash-minimal
                                                  (if (eq? 'podman
                                                           (oci-configuration-runtime config))
                                                      podman
                                                      docker-cli))))
                       (service-extension subids-service-type
                                          oci-service-subids)
                       (service-extension account-service-type
                                          oci-service-accounts)
                       (service-extension shepherd-root-service-type
                                          oci-configuration->shepherd-services)))
                ;; Concatenate OCI object lists.
                (compose (lambda (args)
                           (fold oci-extension-merge
                                 (oci-extension)
                                 args)))
                (extend
                 (lambda (config extension)
                   (oci-configuration
                    (inherit config)
                    (containers (oci-objects-merge-lst
                                 (oci-extension-containers config)
                                 (oci-extension-containers extension)
                                 "container"
                                 (lambda (config)
                                   (define maybe-name (oci-container-configuration-provision config))
                                   (if (maybe-value-set? maybe-name)
                                       (symbol->string maybe-name)
                                       (oci-image->container-name
                                        (oci-container-configuration-image config))))))
                    (networks (oci-objects-merge-lst
                               (oci-extension-networks config)
                               (oci-extension-networks extension)
                               "network"
                               oci-network-shepherd-name))
                    (volumes (oci-objects-merge-lst
                              (oci-extension-volumes config)
                              (oci-extension-volumes extension)
                              "volume"
                              oci-volume-shepherd-name)))))
                (default-value (oci-configuration))
                (description
                 "This service implements the proviosioning of OCI object such
as containers networks and volumes.")))

(define oci-container-service-type
  (service-type (inherit oci-container-service-type)
                (extensions (list (service-extension oci-service-type
                                                     (lambda (containers)
                                                       (oci-extension
                                                        (containers containers))))))))
