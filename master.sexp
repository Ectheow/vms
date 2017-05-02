((ubuntu-copy
(command) (vnc-port . 11) (cdrom . ./ubuntu-16.04.2-server-amd64.iso) (cpus . 4) (drive . "./ubuntu-16.04.img.copy") (tap-id . "tap2") (memory . 4096) (smp . 4))

(vms (ubuntu (command) (vnc-port . 10) (cdrom . ./ubuntu-16.04.2-server-amd64.iso) (cpus . 4) (drive . ./ubuntu-16.04.img) (tap-id . tap1) (memory . 4096) (smp . 4))))