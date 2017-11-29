FILES=\
	boatdirectory-docker

all: copy_to_sorel_ubuntu_server_1
	cd boatdirectory && make $<
	cd boatdirectory-client && make $<

copy_to_ec2_54_218_74_211:
	rsync -RrLztih $(FILES) ec2-54-218-74-211.us-west-2.compute.amazonaws.com:/home/ubuntu/sorel/docker/
	@echo DONE

copy_to_sorel_ubuntu_server_1:
	rsync -RrLztih -e 'ssh -p 2222' $(FILES) localhost:/home/sorel/docker/
	@echo DONE
