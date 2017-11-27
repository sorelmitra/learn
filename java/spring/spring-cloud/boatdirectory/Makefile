FILES=\
	target/boatdirectory-0.0.1-SNAPSHOT.jar \
	Dockerfile

DST=ec2-54-218-74-211.us-west-2.compute.amazonaws.com:/home/ubuntu/sorel/docker/boatdirectory

all: copy_to_remote_server

copy_to_remote_server:
	rsync -RrLztih $(FILES) $(DST)
	@echo DONE
