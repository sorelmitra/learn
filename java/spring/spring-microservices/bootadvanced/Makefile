FILES=\
	target/bootadvanced-0.0.1-SNAPSHOT.jar \
	Dockerfile

DST=sorelmitra@77.81.122.41:/Users/sorelmitra/Documents/docker/bootadvanced/

all: copy_to_luxoft_mac_1

copy_to_luxoft_mac_1:
	rsync -RrLztih $(FILES) $(DST)
	@echo DONE
