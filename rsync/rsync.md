# Sample Command

Options explained:

	-R: keep directory structure
	-r: recursive
	-L: expand symlinks
	-z: compress
	-t: update timestamps on dest
	-i: output a log of changed files
	-h: use human-readable numbers
	-p: preserve file permissions
	--exclude: exclude files matching PATTERN

rsync -rLztih --exclude=autotest_logs --exclude='rel_f*' --exclude='CVS' sqa@10.144.17.20:/volume/private-builds/sorelm/wlan-sim/wDEV_MSS_ALTAIR /w/2local/from_sunray/builds/wlan-sim


# Using a Key File with Rsync

You can specify the exact ssh command via the '-e' option:

	rsync -Pav -e "ssh -i $HOME/.ssh/somekey" username@hostname:/from/dir/ /to/dir/

Even better, you can specify default settings per host via the SSH ~/.ssh/config file:

	Host hostname
	    User username
	    IdentityFile ~/.ssh/somekey
	
In the long run it is best to learn the ~/.ssh/config file.
