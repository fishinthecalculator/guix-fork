(use-modules (gnu tests)
             (guix gexp)
             (gnu packages)
             (gnu system vm)
             (gnu services)
             (gnu services containers)
             (gnu services dbus)
             (gnu services desktop)
             (gnu services docker)
             (gnu services networking))

(virtual-machine
 (operating-system
   ;; (marionette-operating-system
   ;;  (operating-system-with-gc-roots
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
                 (log-file
                  "/var/log/test.log")
                 (entrypoint
                  "/bin/guile")
                 (command
                  '("-c" "(let l ((c 300))(display c)(sleep 1)(when(positive? c)(l (- c 1))))"))
                 (host-environment
                  '(("VARIABLE" . "value")))
                 (volumes
                  '(("/shared.txt" . "/shared.txt:ro")))
                 (extra-arguments
                  '("--env" "VARIABLE"))))))
     ;; (list))
    ;; #:imported-modules '((gnu services herd)
    ;;                      (guix combinators)))
)
 (volatile? #f)
 (memory-size 1024)
 (disk-image-size (* 3000 (expt 2 20)))
 (port-forwardings '()))
