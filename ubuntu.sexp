((command .
	  (sudo kvm -drive file=./ubuntu-16.04.img
	       -boot cd 
	       -vnc :10 
	       -net nic,model=virtio
	       -net tap,id=tap1 
	       -cdrom ./ubuntu-16.04.2-server-amd64.iso 
	       -m 4096 
	       -smp cpus=4)))

