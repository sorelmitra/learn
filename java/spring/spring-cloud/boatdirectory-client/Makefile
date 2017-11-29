FILES=\
	target/boatdirectory-client-0.0.1-SNAPSHOT.jar \
	Dockerfile

all: copy_to_ec2_54_218_74_211 copy_to_sorel_ubuntu_server_1

copy_to_ec2_54_218_74_211: build
	rsync -RrLztih $(FILES) ec2-54-218-74-211.us-west-2.compute.amazonaws.com:/home/ubuntu/sorel/docker/boatdirectory-client/
	@echo DONE

copy_to_sorel_ubuntu_server_1: build
	rsync -RrLztih -e 'ssh -p 2222' $(FILES) localhost:/home/sorel/docker/boatdirectory-client/
	@echo DONE

build:
	mvn -DskipTests package
