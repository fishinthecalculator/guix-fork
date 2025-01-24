;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024, 2025 Giacomo Leidi <goodoldpaul@autistici.org>
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
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (rootless-podman-configuration
            rootless-podman-configuration?
            rootless-podman-configuration-fields
            rootless-podman-configuration-podman
            rootless-podman-configuration-group-name
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

            lower-oci-image-state

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

            list-of-oci-containers?
            list-of-oci-networks?
            list-of-oci-volumes?

            %oci-supported-runtimes
            oci-sanitize-runtime
            oci-runtime-system-environment
            oci-runtime-system-extra-arguments
            oci-runtime-system-group
            oci-runtime-system-requirement
            oci-runtime-cli
            oci-runtime-system-cli
            oci-runtime-home-cli
            oci-runtime-name
            oci-runtime-group

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
            oci-configuration-runtime
            oci-configuration-runtime-cli
            oci-configuration-runtime-extra-arguments
            oci-configuration-user
            oci-configuration-group
            oci-configuration-containers
            oci-configuration-networks
            oci-configuration-volumes
            oci-configuration-verbose?
            oci-configuration-valid?

            oci-extension
            oci-extension?
            oci-extension-fields
            oci-extension-containers
            oci-extension-networks
            oci-extension-volumes

            oci-container-shepherd-name
            oci-networks-shepherd-name
            oci-networks-home-shepherd-name
            oci-volumes-shepherd-name
            oci-volumes-home-shepherd-name

            oci-container-shepherd-service
            oci-objects-merge-lst
            oci-extension-merge
            oci-service-extension-wrap-validate
            oci-service-type
            oci-service-accounts
            oci-service-profile
            oci-service-subids
            oci-state->shepherd-services
            oci-configuration->shepherd-services
            oci-configuration-extend))

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

(define (package-or-#f? val)
  (or (not val)
      (package? val)))

(define-configuration/no-serialization rootless-podman-configuration
  (podman
   (package-or-#f podman)
   "The Podman package that will be installed in the system profile.
@code{#f} can be passed to suppress the installation.")
  (group-name
   (string "cgroup")
   "The name of the group that will own /sys/fs/cgroup resources.  Users that
want to use rootless Podman have to be in this group.")
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

(define rootless-podman-service-profile
  (lambda (config)
    (or (and=> (rootless-podman-configuration-podman config) list)
        (list))))

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
                   (string-append #+bash-minimal "/bin/bash") "-l" "-c"
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
                   (string-append #+bash-minimal "/bin/bash") "-c"
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
                     "Allow setting cgroups limits: cpu, cpuset, io, memory and
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
                   "/run/privileged/bin/mount" "--make-shared" "/")))

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
;;; OCI provisioning service.
;;;

(define %oci-supported-runtimes
  '(docker podman))

(define (oci-runtime-system-requirement runtime)
  "Return a list of Shepherd service names required by a given OCI runtime,
before it is able to run containers."
  (if (eq? 'podman runtime)
      '(cgroups2-fs-owner cgroups2-limits
        rootless-podman-shared-root-fs user-processes)
      '(dockerd user-processes)))

(define (oci-runtime-name runtime)
  "Return a human readable name for a given OCI runtime."
  (if (eq? 'podman runtime)
      "Podman" "Docker"))

(define (oci-runtime-group runtime maybe-group)
  "Implement the logic behind selection of the group that is to be used by
Shepherd to execute OCI commands."
  (if (eq? maybe-group #f)
      (if (eq? 'podman runtime)
          "cgroup"
          "docker")
      maybe-group))

(define (oci-sanitize-runtime value)
  (unless (member value %oci-supported-runtimes)
    (raise
     (formatted-message
      (G_ "OCI runtime must be a symbol and one of ~a,
but ~a was found") %oci-supported-runtimes value)))
  value)

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
  "Return a string OCI image reference representing IMAGE."
  (define reference
    (if (string? image)
        image
        (string-append (oci-image-repository image)
                       ":" (oci-image-tag image))))
  (if (> (length (string-split reference #\/)) 1)
        reference
        (string-append "localhost/" reference)))

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

(define-maybe/no-serialization string)
(define-maybe/no-serialization package)
(define-maybe/no-serialization subid-range)

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
   (maybe-string)
   "The user name under whose authority OCI commands will be run.  This field will
override the @code{user} field of @code{oci-configuration}.")
  (group
   (maybe-string)
   "The group name under whose authority OCI commands will be run.  When
using the @code{'podman} OCI runtime, this field will be ignored and the
default group of the user configured in the @code{user} field will be used.
This field will override the @code{group} field of @code{oci-configuration}.")
  (command
   (list-of-strings '())
   "Overwrite the default command (@code{CMD}) of the image.")
  (entrypoint
   (maybe-string)
   "Overwrite the default entrypoint (@code{ENTRYPOINT}) of the image.")
  (host-environment
   (list '())
   "Set environment variables in the host environment where @command{docker run}
or @command{podman run} are invoked.  This is especially useful to pass secrets
from the host to the container without having them on the OCI runtime command line,
for example: by setting the @code{MYSQL_PASSWORD} on the host and by passing
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
directly to the OCI runtime CLI.  You can refer to the
@url{https://docs.docker.com/engine/reference/commandline/run/#env,Docker}
or @url{https://docs.podman.io/en/stable/markdown/podman-run.1.html#env-e-env,Podman}
upstream documentation for semantics."
   (sanitizer oci-sanitize-environment))
  (image
   (string-or-oci-image)
   "The image used to build the container.  It can be a string or an
@code{oci-image} record.  Strings are resolved by the OCI runtime,
and follow the usual format
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
   "Set an OCI network for the spawned container.")
  (ports
   (list '())
   "Set the port or port ranges to expose from the spawned container.  This can
be a list of pairs or strings, even mixed:

@lisp
(list '(\"8080\" . \"80\")
      \"10443:443\")
@end lisp

Pair members can be strings, gexps or file-like objects. Strings are passed
directly to the OCI runtime CLI.  You can refer to the
@url{https://docs.docker.com/engine/reference/commandline/run/#publish,Docker}
or @url{https://docs.podman.io/en/stable/markdown/podman-run.1.html#publish-p-ip-hostport-containerport-protocol,Podman}
upstream documentation for semantics."
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
directly to the OCI runtime CLI.  You can refer to the
@url{https://docs.docker.com/engine/reference/commandline/run/#volume,Docker}
or @url{https://docs.podman.io/en/stable/markdown/podman-run.1.html#volume-v-source-volume-host-dir-container-dir-options,Podman}
upstream documentation for semantics."
   (sanitizer oci-sanitize-volumes))
  (container-user
   (maybe-string)
   "Set the current user inside the spawned container.  You can refer to the
@url{https://docs.docker.com/engine/reference/run/#user,Docker}
or @url{https://docs.podman.io/en/stable/markdown/podman-run.1.html#user-u-user-group,Podman}
upstream documentation for semantics.")
  (workdir
   (maybe-string)
   "Set the current working directory for the spawned Shepherd service.
You can refer to the
@url{https://docs.docker.com/engine/reference/run/#workdir,Docker}
or @url{https://docs.podman.io/en/stable/markdown/podman-run.1.html#workdir-w-dir,Podman}
upstream documentation for semantics.")
  (extra-arguments
   (list '())
   "A list of strings, gexps or file-like objects that will be directly passed
to the @command{docker run} or @command{podman run} invokation."
   (sanitizer oci-sanitize-extra-arguments)))

(define (list-of-oci-containers? value)
  (list-of-oci-records? "containers" oci-container-configuration? value))

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
to the @command{docker volume create} or @command{podman volume create}
invokation."
   (sanitizer oci-sanitize-extra-arguments)))

(define (list-of-oci-volumes? value)
  (list-of-oci-records? "volumes" oci-volume-configuration? value))

(define-configuration/no-serialization oci-network-configuration
  (name
   (string)
   "The name of the OCI network to provision.")
  (driver
   (maybe-string)
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
   (maybe-string)
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
to the @command{docker network create} or @command{podman network create}
invokation."
   (sanitizer oci-sanitize-extra-arguments)))

(define (list-of-oci-networks? value)
  (list-of-oci-records? "networks" oci-network-configuration? value))

(define-record-type* <oci-configuration>
  oci-configuration
  make-oci-configuration
  oci-configuration?
  this-oci-configuration

  (runtime                  oci-configuration-runtime
                            (default 'docker))
  (runtime-cli              oci-configuration-runtime-cli
                            (default #f))                               ; package or string
  (runtime-extra-arguments  oci-configuration-runtime-extra-arguments   ; strings or gexps
                            (default '()))                              ; or file-like objects
  (user                     oci-configuration-user
                            (default "oci-container"))
  (group                    oci-configuration-group                     ; string
                            (default #f))
  (subuids-range            oci-configuration-subuids-range             ; subid-range
                            (default #f))
  (subgids-range            oci-configuration-subgids-range             ; subid-range
                            (default #f))
  (containers               oci-configuration-containers                ; oci-container-configurations
                            (default '()))
  (networks                 oci-configuration-networks                  ; oci-network-configurations
                            (default '()))
  (volumes                  oci-configuration-volumes                   ; oci-volume-configurations
                            (default '()))
  (verbose?                 oci-configuration-verbose?
                            (default #f))
  (home-service?            oci-configuration-home-service?
                            (default for-home?) (innate)))

(define (package-or-string? value)
  (or (package? value) (string? value)))

(define (oci-configuration-valid? config)
  (define runtime-cli
    (oci-configuration-runtime-cli config))
  (define group
    (oci-configuration-group config))
  (define subuids-range
    (oci-configuration-subuids-range config))
  (define subgids-range
    (oci-configuration-subgids-range config))
  (and
   (symbol?
    (oci-sanitize-runtime (oci-configuration-runtime config)))
   (or (eq? runtime-cli #f)
       (package-or-string? runtime-cli))
   (list? (oci-configuration-runtime-extra-arguments config))
   (string? (oci-configuration-user config))
   (or (eq? group #f)
       (string? group))
   (or (eq? subuids-range #f)
       (subid-range? subuids-range))
   (or (eq? subgids-range #f)
       (subid-range? subgids-range))
   (list-of-oci-containers?
    (oci-configuration-containers config))
   (list-of-oci-networks?
    (oci-configuration-networks config))
   (list-of-oci-volumes?
    (oci-configuration-volumes config))
   (boolean?
    (oci-configuration-verbose? config))
   (boolean?
    (oci-configuration-home-service? config))))

(define (oci-runtime-system-environment runtime user)
  (if (eq? runtime 'podman)
      (list
       #~(string-append
          "HOME=" (passwd:dir (getpwnam #$user))))
      #~()))

(define (oci-runtime-system-group runtime user group)
  (if (eq? runtime 'podman)
      #~(group:name
         (getgrgid
          (passwd:gid
           (getpwnam #$user))))
      group))

(define (oci-runtime-cli runtime runtime-cli path)
  "Return a gexp that, when lowered, evaluates to the file system path of the OCI
runtime command requested by the user."
  (if (string? runtime-cli)
      ;; It is a user defined absolute path
      runtime-cli
      #~(string-append
         #$(if (eq? runtime-cli #f)
               path
               runtime-cli)
         #$(if (eq? 'podman runtime)
               "/bin/podman"
               "/bin/docker"))))

(define* (oci-runtime-system-cli config #:key (path "/run/current-system/profile"))
  (let ((runtime-cli
         (oci-configuration-runtime-cli config))
        (runtime
         (oci-configuration-runtime config)))
    (oci-runtime-cli runtime runtime-cli path)))

(define (oci-runtime-home-cli config)
  (let ((runtime-cli
         (oci-configuration-runtime-cli config))
        (runtime
         (oci-configuration-runtime config)))
    (oci-runtime-cli runtime runtime-cli
                     (string-append (getenv "HOME")
                                    "/.guix-home/profile"))))

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
  "Infer the name of an OCI backed Shepherd service from its OCI image."
  (basename
   (if (string? image)
       (first (string-split image #\:))
       (oci-image-repository image))))

(define (oci-object-command-shepherd-action object-name invokation)
  "Return a Shepherd action printing a given INVOKATION of an OCI command for the
given OBJECT-NAME."
  (shepherd-action
   (name 'command-line)
   (documentation
    (format #f "Prints ~a's OCI runtime command line invokation."
            object-name))
   (procedure
    #~(lambda _
        (format #t "~a~%" #$invokation)))))

(define (oci-container-shepherd-name runtime config)
  "Return the name of an OCI backed Shepherd service based on CONFIG.
The name configured in the configuration record is returned when
CONFIG's name field has a value, otherwise a name is inferred from CONFIG's
image field."
  (define name (oci-container-configuration-provision config))
  (define image (oci-container-configuration-image config))

  (if (maybe-value-set? name)
      name
      (string-append (symbol->string runtime) "-"
                     (oci-image->container-name image))))

(define (oci-networks-shepherd-name runtime)
  "Return the name of the OCI networks provisioning Shepherd service based on
RUNTIME."
  (string-append (symbol->string runtime) "-networks"))

(define (oci-volumes-shepherd-name runtime)
  "Return the name of the OCI volumes provisioning Shepherd service based on
RUNTIME."
  (string-append (symbol->string runtime) "-volumes"))

(define (oci-networks-home-shepherd-name runtime)
  "Return the name of the OCI volumes provisioning Home Shepherd service based on
RUNTIME."
  (string-append "home-" (oci-networks-shepherd-name runtime)))

(define (oci-volumes-home-shepherd-name runtime)
  "Return the name of the OCI volumes provisioning Home Shepherd service based on
RUNTIME."
  (string-append "home-" (oci-volumes-shepherd-name runtime)))

(define (oci-container-configuration->options config)
  "Map CONFIG, an oci-container-configuration record, to a gexp that, upon
lowering, will be evaluated to a list of strings containing command line options
for the OCI runtime run command."
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
                       (oci-container-configuration-volumes config)))))))

(define (oci-network-configuration->options config)
  "Map CONFIG, an oci-network-configuration record, to a gexp that, upon
lowering, will be evaluated to a list of strings containing command line options
for the OCI runtime network create command."
  (let ((driver (oci-network-configuration-driver config))
        (gateway
         (oci-network-configuration-gateway config))
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
                   `(,(if (maybe-value-set? driver)
                          `("--driver" ,driver)
                          '())
                     ,(if (maybe-value-set? gateway)
                          `("--gateway" ,gateway)
                          '())
                     ,(if internal?
                          `("--internal")
                          '())
                     ,(if (maybe-value-set? ip-range)
                          `("--ip-range" ,ip-range)
                          '())
                     ,(if (maybe-value-set? ipam-driver)
                          `("--ipam-driver" ,ipam-driver)
                          '())
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
  "Map CONFIG, an oci-volume-configuration record, to a gexp that, upon
lowering, will be evaluated to a list of strings containing command line options
for the OCI runtime volume create command."
  (append-map
   (lambda (spec)
     (list "--label" spec))
   (oci-volume-configuration-labels config)))

(define (lower-operating-system os target system)
  "Lower OS, an operating-system record, into a tarball containing an OCI image."
  (mlet* %store-monad
      ((tarball
        (lower-object
         (system-image (os->image os #:type docker-image-type))
         system
         #:target target)))
    (return tarball)))

(define (lower-manifest name value options image-reference
                        target system grafts?)
  "Lower VALUE, a manifest record, into a tarball containing an OCI image."
  (mlet* %store-monad
      ((_ (set-grafting grafts?))
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
                         #:localstatedir? #t))))
    (return tarball)))

(define (lower-oci-image-state name value options reference
                               image-target image-system grafts?)
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
        (lower-manifest name value options reference
                        target system grafts?))
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

(define (lower-oci-image name image)
  "Lower IMAGE, a oci-image record, into a tarball containing an OCI image."
  (lower-oci-image-state
   name
   (oci-image-value image)
   (oci-image-pack-options image)
   (oci-image-reference image)
   (oci-image-target image)
   (oci-image-system image)
   (oci-image-grafts? image)))

(define (oci-object-exists? runtime runtime-cli object verbose?)
  #~(lambda* (name #:key (format-string "{{.Name}}"))
      (use-modules (ice-9 format)
                   (ice-9 match)
                   (ice-9 popen)
                   (ice-9 rdelim)
                   (srfi srfi-1))

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

      #$(if (eq? runtime 'podman)
            #~(let ((command
                     (list #$runtime-cli
                           #$object "exists" name)))
                (when #$verbose?
                  (format #t "Running~{ ~a~}~%" command))
                (define exit-code (status:exit-val (apply system* command)))
                (when #$verbose?
                  (format #t "Exit code: ~a~%" exit-code))
                (equal? EXIT_SUCCESS exit-code))
            #~(let ((command
                     (string-append #$runtime-cli
                                    " " #$object " ls --format "
                                    "\"" format-string "\"")))
                (when #$verbose?
                  (format #t "Running ~a~%" command))
                (member name (read-lines (open-input-pipe command)))))))

(define* (oci-image-loader runtime runtime-cli name image tag #:key (verbose? #f))
  "Return a file-like object that, once lowered, will evaluate to a program able
to load IMAGE through RUNTIME-CLI and to tag it with TAG afterwards."
  (let ((tarball (lower-oci-image name image)))
    (with-imported-modules '((guix build utils))
      (program-file
       (format #f "~a-image-loader" name)
       #~(begin
           (use-modules (guix build utils)
                        (ice-9 match)
                        (ice-9 popen)
                        (ice-9 rdelim)
                        (srfi srfi-1))
           (define object-exists?
             #$(oci-object-exists? runtime runtime-cli "image" verbose?))
           (define load-command
             (string-append #$runtime-cli
                            " load -i " #$tarball))

           (if (object-exists? #$tag #:format-string "{{.Repository}}:{{.Tag}}")
               (format #t "~a image already exists, skipping.~%" #$tag)
               (begin
                 (format #t "Loading image for ~a from ~a...~%" #$name #$tarball)
                 (when #$verbose?
                   (format #t "Running ~a~%" load-command))
                 (let ((line (read-line
                              (open-input-pipe load-command))))
                   (unless (or (eof-object? line)
                               (string-null? line))
                     (format #t "~a~%" line)
                     (let* ((repository&tag
                             (string-drop line
                                          (string-length
                                           "Loaded image: ")))
                            (tag-command
                             (list #$runtime-cli "tag" repository&tag #$tag))
                            (drop-old-tag-command
                             (list #$runtime-cli "image" "rm" "-f" repository&tag)))

                       (unless (string=? repository&tag #$tag)
                         (when #$verbose?
                           (format #t "Running~{ ~a~}~%" tag-command))

                         (let ((exit-code
                                (status:exit-val (apply system* tag-command))))
                           (format #t "Tagged ~a with ~a...~%" #$tarball #$tag)

                           (when #$verbose?
                             (format #t "Exit code: ~a~%" exit-code))

                           (when (equal? EXIT_SUCCESS exit-code)
                             (when #$verbose?
                               (format #t "Running~{ ~a~}~%" drop-old-tag-command))
                             (let ((drop-exit-code
                                    (status:exit-val (apply system* drop-old-tag-command))))
                               (when #$verbose?
                                 (format #t "Exit code: ~a~%" drop-exit-code))))))))))))))))

(define (oci-container-run-invokation runtime-cli name command image-reference
                                      options runtime-extra-arguments run-extra-arguments)
  "Return a list representing the OCI runtime
invokation for running containers."
  ;; run [OPTIONS] IMAGE [COMMAND] [ARG...]
  `(,runtime-cli ,@runtime-extra-arguments "run"
    "--rm" "--name" ,name
    ,@options ,@run-extra-arguments
    ,image-reference ,@command))

(define* (oci-container-entrypoint runtime runtime-cli name image image-reference
                                   invokation #:key (verbose? #f))
  "Return a file-like object that, once lowered, will evaluate to the entrypoint
for the Shepherd service that will run IMAGE through RUNTIME-CLI."
  (program-file
   (string-append "oci-entrypoint-" name)
   #~(begin
       (use-modules (ice-9 format)
                    (srfi srfi-1))
       (when #$verbose?
         (format #t "Running in verbose mode...~%"))
       (define invokation (list #$@invokation))
       #$@(if (oci-image? image)
              #~((system*
                  #$(oci-image-loader
                     runtime runtime-cli name image
                     image-reference #:verbose? verbose?)))
              #~())
       (when #$verbose?
         (format #t "Running~{ ~a~}~%" invokation))
       (apply execlp `(,(first invokation) ,@invokation)))))

(define* (oci-container-shepherd-service runtime runtime-cli config
                                         #:key
                                         (runtime-environment #~())
                                         (runtime-extra-arguments '())
                                         (oci-requirement '())
                                         (user #f)
                                         (group #f)
                                         (verbose? #f))
  "Return a Shepherd service object that will run the OCI container represented
by CONFIG through RUNTIME-CLI."
  (let* ((actions (oci-container-configuration-shepherd-actions config))
         (auto-start?
          (oci-container-configuration-auto-start? config))
         (maybe-user (oci-container-configuration-user config))
         (maybe-group (oci-container-configuration-group config))
         (user (if (maybe-value-set? maybe-user)
                    maybe-user
                    user))
         (group (if (maybe-value-set? maybe-group)
                    maybe-group
                    group))
         (host-environment
          (oci-container-configuration-host-environment config))
         (command (oci-container-configuration-command config))
         (log-file (oci-container-configuration-log-file config))
         (requirement (oci-container-configuration-requirement config))
         (respawn?
          (oci-container-configuration-respawn? config))
         (image (oci-container-configuration-image config))
         (image-reference (oci-image-reference image))
         (options (oci-container-configuration->options config))
         (name
          (oci-container-shepherd-name runtime config))
         (extra-arguments
          (oci-container-configuration-extra-arguments config))
         (invokation
          (oci-container-run-invokation
           runtime-cli name command image-reference
           options runtime-extra-arguments extra-arguments))
         (container-action
          (lambda* (command #:key (environment-variables #f))
            #~(lambda _
                (fork+exec-command
                 (list
                  #$@command)
                 #$@(if user (list #:user user) '())
                 #$@(if group (list #:group group) '())
                 #$@(if (maybe-value-set? log-file)
                        (list #:log-file log-file)
                        '())
                 #$@(if (and user (eq? runtime 'podman))
                        (list #:directory
                              #~(passwd:dir (getpwnam #$user)))
                        '())
                 #$@(if environment-variables
                        (list #:environment-variables
                              environment-variables)
                        '()))))))

    (shepherd-service (provision `(,(string->symbol name)))
                      (requirement `(,@oci-requirement
                                     ,@requirement))
                      (respawn? respawn?)
                      (auto-start? auto-start?)
                      (documentation
                       (string-append
                        (oci-runtime-name runtime) " backed Shepherd service for "
                        (if (oci-image? image) name image) "."))
                      (start
                       (container-action
                        (list (oci-container-entrypoint
                               runtime runtime-cli name image image-reference
                               invokation #:verbose? verbose?))
                        #:environment-variables
                        #~(append
                           (list #$@host-environment)
                           (list #$@runtime-environment))))
                      (stop
                       (container-action
                        (list
                         runtime-cli "rm" "-f" name)))
                      (actions
                       (append
                        (list
                         (oci-object-command-shepherd-action
                          name #~(string-join (list #$@invokation) " ")))
                        (if (oci-image? image)
                            '()
                            (list
                             (let ((service-name name))
                               (shepherd-action
                                (name 'pull)
                                (documentation
                                 (format #f "Pull ~a's image (~a)."
                                         service-name image))
                                (procedure
                                 (container-action
                                  (list
                                   runtime-cli "pull" image)))))))
                        actions)))))

(define (oci-object-create-invokation object runtime-cli name options
                                      runtime-extra-arguments
                                      create-extra-arguments)
  "Return a gexp that, upon lowering, will evaluate to the OCI runtime
invokation for creating networks and volumes."
  ;; network|volume create [options] [NAME]
  #~(list #$runtime-cli #$@runtime-extra-arguments #$object "create"
          #$@options #$@create-extra-arguments #$name))

(define (format-oci-invokations invokations)
  "Return a gexp that, upon lowering, will evaluate to a formatted message
containing the INVOKATIONS that the OCI runtime will execute to provision
networks or volumes."
  #~(string-join (map (lambda (i) (string-join i " "))
                      (list #$@invokations))
                 "\n"))

(define* (oci-object-create-script object runtime runtime-cli invokations
                                   #:key (verbose? #f))
  "Return a file-like object that, once lowered, will evaluate to a program able
to create OCI networks and volumes through RUNTIME-CLI."
  (define runtime-string (symbol->string runtime))
  (program-file
   (string-append runtime-string "-" object "s-create.scm")
   #~(begin
       (use-modules (ice-9 format)
                    (ice-9 match)
                    (ice-9 popen)
                    (ice-9 rdelim)
                    (srfi srfi-1))

       (define object-exists?
         #$(oci-object-exists? runtime runtime-cli object verbose?))

       (for-each
        (lambda (invokation)
          (define name (last invokation))
          (if (object-exists? name)
              (format #t "~a ~a ~a already exists, skipping creation.~%"
                      #$(oci-runtime-name runtime) name #$object)
              (begin
                (when #$verbose?
                  (format #t "Running~{ ~a~}~%" invokation))
                (let ((exit-code (status:exit-val (apply system* invokation))))
                  (when #$verbose?
                    (format #t "Exit code: ~a~%" exit-code))))))
        (list #$@invokations)))))

(define* (oci-object-shepherd-service object runtime runtime-cli name requirement invokations
                                      #:key
                                      (runtime-environment #~())
                                      (user #f)
                                      (group #f)
                                      (verbose? #f))
  "Return a Shepherd service object that will create the OBJECTs represented
by INVOKATIONS through RUNTIME-CLI."
  (shepherd-service (provision `(,(string->symbol name)))
                    (requirement requirement)
                    (one-shot? #t)
                    (documentation
                     (string-append
                      (oci-runtime-name runtime) " " object
                      " provisioning service"))
                    (start
                     #~(lambda _
                         (fork+exec-command
                          (list
                           #$(oci-object-create-script
                              object runtime runtime-cli
                              invokations
                              #:verbose? verbose?))
                          #$@(if user (list #:user user) '())
                          #$@(if group (list #:group group) '())
                          #:environment-variables
                          (list #$@runtime-environment))))
                    (actions
                     (list
                      (oci-object-command-shepherd-action
                       name (format-oci-invokations invokations))))))

(define* (oci-networks-shepherd-service runtime runtime-cli name networks
                                        #:key (user #f) (group #f) (verbose? #f)
                                        (runtime-extra-arguments '())
                                        (runtime-environment #~())
                                        (runtime-requirement '()))
  "Return a Shepherd service object that will create the networks represented
in CONFIG."
  (let ((invokations
         (map
          (lambda (network)
            (oci-object-create-invokation
             "network" runtime-cli
             (oci-network-configuration-name network)
             (oci-network-configuration->options network)
             runtime-extra-arguments
             (oci-network-configuration-extra-arguments network)))
          networks)))

    (oci-object-shepherd-service
     "network" runtime runtime-cli name
     runtime-requirement invokations
     #:user user #:group group #:runtime-environment runtime-environment
     #:verbose? verbose?)))

(define* (oci-volumes-shepherd-service runtime runtime-cli name volumes
                                       #:key (user #f) (group #f) (verbose? #f)
                                       (runtime-extra-arguments '())
                                       (runtime-environment #~())
                                       (runtime-requirement '()))
  "Return a Shepherd service object that will create the volumes represented
in CONFIG."
  (let ((invokations
         (map
          (lambda (volume)
            (oci-object-create-invokation
             "volume" runtime-cli
             (oci-volume-configuration-name volume)
             (oci-volume-configuration->options volume)
             runtime-extra-arguments
             (oci-volume-configuration-extra-arguments volume)))
          volumes)))

    (oci-object-shepherd-service
     "volume" runtime runtime-cli name runtime-requirement invokations
     #:user user #:group group #:runtime-environment runtime-environment
     #:verbose? verbose?)))

(define (oci-service-accounts config)
  (define user (oci-configuration-user config))
  (define maybe-group (oci-configuration-group config))
  (define runtime (oci-configuration-runtime config))
  (list (user-account
         (name user)
         (comment "OCI services account")
         (group "users")
         (supplementary-groups
          (list (oci-runtime-group runtime maybe-group)))
         (system? (eq? 'docker runtime))
         (home-directory (if (eq? 'podman runtime)
                             (string-append "/home/" user)
                             "/var/empty"))
         (create-home-directory? (eq? 'podman runtime))
         (shell (file-append shadow "/sbin/nologin")))))

(define* (oci-state->shepherd-services runtime runtime-cli containers networks volumes
                                       #:key (user #f) (group #f) (verbose? #f)
                                       (networks-name #f) (volumes-name #f)
                                       (runtime-extra-arguments '())
                                       (runtime-environment #~())
                                       (runtime-requirement '())
                                       (containers-requirement '())
                                       (networks-requirement '())
                                       (volumes-requirement '()))
  (let* ((networks-name
          (if (string? networks-name)
              networks-name
              (oci-networks-shepherd-name runtime)))
         (networks?
          (> (length networks) 0))
         (networks-service
          (if networks?
              (list
               (string->symbol networks-name))
              '()))
         (volumes-name
          (if (string? volumes-name)
              volumes-name
              (oci-volumes-shepherd-name runtime)))
         (volumes?
          (> (length volumes) 0))
         (volumes-service
          (if volumes?
              (list (string->symbol volumes-name))
              '())))
    (append
     (map
      (lambda (c)
        (oci-container-shepherd-service
         runtime runtime-cli c
         #:user user
         #:group group
         #:runtime-environment runtime-environment
         #:runtime-extra-arguments runtime-extra-arguments
         #:oci-requirement
         (append containers-requirement
                 runtime-requirement
                 networks-service
                 volumes-service)
         #:verbose? verbose?))
      containers)
     (if networks?
         (list
          (oci-networks-shepherd-service
           runtime runtime-cli
           networks-name networks
           #:user user #:group group
           #:runtime-extra-arguments runtime-extra-arguments
           #:runtime-environment runtime-environment
           #:runtime-requirement (append networks-requirement
                                         runtime-requirement)
           #:verbose? verbose?))
         '())
     (if volumes?
         (list
          (oci-volumes-shepherd-service
           runtime runtime-cli
           volumes-name volumes
           #:user user #:group group
           #:runtime-extra-arguments runtime-extra-arguments
           #:runtime-environment runtime-environment
           #:runtime-requirement (append runtime-requirement
                                         volumes-requirement)
           #:verbose? verbose?))
         '()))))

(define (oci-configuration->shepherd-services config)
  (let* ((runtime (oci-configuration-runtime config))
         (system-runtime-cli
          (oci-runtime-system-cli config))
         (home-runtime-cli
          (oci-runtime-home-cli config))
         (runtime-extra-arguments
          (oci-configuration-runtime-extra-arguments config))
         (containers (oci-configuration-containers config))
         (networks (oci-configuration-networks config))
         (volumes (oci-configuration-volumes config))
         (user (oci-configuration-user config))
         (group (oci-runtime-group
                 runtime (oci-configuration-group config)))
         (verbose? (oci-configuration-verbose? config))
         (home-service?
          (oci-configuration-home-service? config)))
    (if home-service?
        (oci-state->shepherd-services runtime home-runtime-cli containers networks volumes
                                      #:verbose? verbose?
                                      #:networks-name
                                      (oci-networks-home-shepherd-name runtime)
                                      #:volumes-name
                                      (oci-volumes-home-shepherd-name runtime))
        (oci-state->shepherd-services runtime system-runtime-cli containers networks volumes
                                      #:user user
                                      #:group
                                      (oci-runtime-system-group runtime user group)
                                      #:verbose? verbose?
                                      #:runtime-extra-arguments
                                      runtime-extra-arguments
                                      #:runtime-environment
                                      (oci-runtime-system-environment runtime user)
                                      #:runtime-requirement
                                      (oci-runtime-system-requirement runtime)
                                      #:networks-requirement '(networking)))))

(define (oci-service-subids config)
  "Return a subids-extension record representing subuids and subgids required by
the rootless Podman backend."
  (define (delete-duplicate-ranges ranges)
    (delete-duplicates ranges
                       (lambda args
                         (apply string=? (map subid-range-name ranges)))))
  (define runtime
    (oci-configuration-runtime config))
  (define user
    (oci-configuration-user config))
  (define subgids (oci-configuration-subgids-range config))
  (define subuids (oci-configuration-subuids-range config))
  (define container-users
    (filter (lambda (range)
              (and (maybe-value-set?
                    (subid-range-name range))
                   (not (string=? (subid-range-name range) user))))
            (map (lambda (container)
                   (subid-range
                    (name
                     (oci-container-configuration-user container))))
                 (oci-configuration-containers config))))
  (define subgid-ranges
    (delete-duplicate-ranges
     (cons
      (if (eq? subgids #f)
          (subid-range (name user))
          subgids)
      container-users)))
  (define subuid-ranges
    (delete-duplicate-ranges
     (cons
      (if (eq? subuids #f)
          (subid-range (name user))
          subuids)
      container-users)))

  (if (eq? 'podman runtime)
      (subids-extension
       (subgids
        subgid-ranges)
       (subuids
        subuid-ranges))
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
remove the duplicate.") object (get-name element) object)))
           (cons element merged))
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
                      maybe-name
                      (oci-image->container-name
                       (oci-container-configuration-image config))))))
   (networks (oci-objects-merge-lst
              (oci-extension-networks a)
              (oci-extension-networks b)
              "network"
              oci-network-configuration-name))
   (volumes (oci-objects-merge-lst
             (oci-extension-volumes a)
             (oci-extension-volumes b)
             "volume"
             oci-volume-configuration-name))))

(define (oci-service-profile runtime runtime-cli)
  `(,bash-minimal
    ,@(if (string? runtime-cli)
          '()
          (list
           (cond
            ((not (eq? runtime-cli #f))
             runtime-cli)
            ((eq? 'podman runtime)
             podman)
            (else
             docker-cli))))))

(define (oci-service-extension-wrap-validate extension)
  (lambda (config)
    (if (oci-configuration-valid? config)
        (extension config)
        (raise
         (formatted-message
          (G_ "Invalide oci-configuration ~a.") config)))))

(define (oci-configuration-extend config extension)
  (oci-configuration
   (inherit config)
   (containers
    (oci-objects-merge-lst
     (oci-configuration-containers config)
     (oci-extension-containers extension)
     "container"
     (lambda (oci-config)
       (define runtime
         (oci-configuration-runtime config))
       (oci-container-shepherd-name runtime oci-config))))
   (networks (oci-objects-merge-lst
              (oci-configuration-networks config)
              (oci-extension-networks extension)
              "network"
              oci-network-configuration-name))
   (volumes (oci-objects-merge-lst
             (oci-configuration-volumes config)
             (oci-extension-volumes extension)
             "volume"
             oci-volume-configuration-name))))

(define oci-service-type
  (service-type (name 'oci)
                (extensions
                 (list
                  (service-extension profile-service-type
                                     (oci-service-extension-wrap-validate
                                      (lambda (config)
                                        (let ((runtime-cli
                                               (oci-configuration-runtime-cli config))
                                              (runtime
                                               (oci-configuration-runtime config)))
                                          (oci-service-profile runtime runtime-cli)))))
                  (service-extension subids-service-type
                                     (oci-service-extension-wrap-validate
                                      oci-service-subids))
                  (service-extension account-service-type
                                     (oci-service-extension-wrap-validate
                                      oci-service-accounts))
                  (service-extension shepherd-root-service-type
                                     (oci-service-extension-wrap-validate
                                      oci-configuration->shepherd-services))))
                ;; Concatenate OCI object lists.
                (compose (lambda (args)
                           (fold oci-extension-merge
                                 (oci-extension)
                                 args)))
                (extend oci-configuration-extend)
                (default-value (oci-configuration))
                (description
                 "This service implements the provisioning of OCI object such
as containers, networks and volumes.")))
