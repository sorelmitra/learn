# Simple function to do a rsync on a remote server that's only accessible via
# another "middle" remote server (thus the "m" in it's name).
# Use case: You're in VPN 1 and want to access a server in VPN 2 without
# switching VPNs all the time. If you have a "middle" server in VPN 2 that's
# still accessible from VPN 1 (UNIX'es allow this), then you can use that
# middle server to do a temporary sync on it.
# This is what this small function does.
mrsync() {
	mip=77.81.122.41
	mdir=/tmp/mrsync/
	middle=sorel@$mip:$mdir

	array=("$@")
	
	index=("${!array[@]}")
	dest=${array[${index[@]: -1}]}
	unset 'array[${index[@]: -1}]'
	
	index=("${!array[@]}")
	source=${array[${index[@]: -1}]}
	unset 'array[${index[@]: -1}]'

	ssh $mip "mkdir -p $mdir" && rsync $array -q $source $middle && ssh $mip "rsync $array ${mdir}* $dest; rm -rf $mdir"
}


