# Call local endpoint from a Docker container

Replace `http://localhost:<port>` with `http://host.docker.internal:<port>`.  That's because for a Docker container, `localhost` points inside the container.

https://stackoverflow.com/a/62526792/6239668

# Docker-Machine with VPN and Private Registries

## For the VPN

First, use Cisco OpenConnect (available in homebrew) with vpn-slice as per https://gist.github.com/stefancocora/686bbce938f27ef72649a181e7bd0158

Then follow the steps in vpn_linux.txt - the Mac variants.

Now docker should work normally while being able to access the VPN resources.

## For Docker-Machine - Allow Insecure Registries

First, create and start your machine as usual.

Then, follow these steps:

1. open c:\users\<user-name>\.docker\machine\machines\default\config.json or ~/.docker/machine/machines/default/config.json in an editor
2. find the InsecureRegistry property and append "localhost:5000" to it
3. go to your docker terminal
4. docker-machine provision default
5. once complete, run docker info
6. you should see your modification to the insecure registries (e.g. in the truncated output below)
		ID: OIKM:PIVF:VPVC:OUV6:SREK:FS4W:6OF4:M2YY:DG2R:NINO:QE2D:U6EQ
		Docker Root Dir: /mnt/sda1/var/lib/docker
		Debug Mode (client): false
		Debug Mode (server): true
		File Descriptors: 16
		Goroutines: 25
		System Time: 2017-08-04T14:50:05.335227242Z
		EventsListeners: 0
		Registry: https://index.docker.io/v1/
		Labels:
		provider=virtualbox
		Experimental: false
		Insecure Registries:
		localhost:5000
		127.0.0.0/8
		Live Restore Enabled: false

Now `docker login` should work.

# Publish Docker Image Directly

	docker save conversation-state-manager:latest | bzip2 | pv | ssh ec2-52-40-7-200.us-west-2.compute.amazonaws.com "bunzip2 | docker load"

Sometimes if the target machine is using docker-machine, you need to re-export all variables and call docker by path like this:

	docker save conversation-state-manager:latest | bzip2 |  ssh 192.168.100.2 "bunzip2 | DOCKER_HOST="tcp://192.168.99.102:2376" DOCKER_TLS_VERIFY="1" DOCKER_CERT_PATH="/Users/sorel/.docker/machine/machines/default" DOCKER_MACHINE_NAME="default" /usr/local/bin/docker load"

# Get monochrome logs from docker

	docker-compose logs --no-color --tail 10 conversation-state-manager > cbank_interest_csm_old.log

# Docker on Mac/Windows that don't have virtualization

Use Docker Toolbox: https://docs.docker.com/toolbox/toolbox_install_mac

# Docker from Maven

If you want to build a docker image with a FROM image that's local, add this to the Maven Docker Plugin:

	<pullNewerImage>false</pullNewerImage>

# Docker Permission Denied

You are trying to run a docker container or do the docker tutorial, but you only get an error message like this:

docker: Got permission denied while trying to connect to the Docker daemon socket at unix:///var/run/docker.sock: Post http://%2Fvar%2Frun%2Fdocker.sock/v1.26/containers/create: dial unix /var/run/docker.sock: connect: permission denied.
See 'docker run --help'.
 Solution:

The error message tells you that your current user can’t access the docker engine, because you’re lacking permissions to access the unix socket to communicate with the engine.

As a temporary solution, you can use sudo to run the failed command as root.
However it is recommended to fix the issue by adding the current user to the docker group:

Run this command in your favourite shell and then completely log out of your account and log back in (if in doubt, reboot!):

sudo usermod -a -G docker $USER

After doing that, you should be able to run the command without any issues. Run docker run hello-world as a normal user in order to check if it works. Reboot if the issue still persists.

Logging out and logging back in is required because the group change will not have an effect unless your session is closed.



# Create Docker Overlay Network, with Bridged Docker-Machine

Source: https://github.com/docker/machine/issues/1491#issuecomment-172325129

1. Create docker machine

docker-machine create default

2. Stop docker machine

docker-machine stop default

3. Add bridged adapter to default docker-machine [3]

a) Open the machine in Virtual Box and add a 3rd new bridge network. Do not modify the existing NAT or host network, this will break Docker. Do this when the machine is stopped (docker-machine stop default)

b) Start the machine - docker-machine start default - You'll see an error that the machine has a new IP.

c) Run eval $(docker-machine env default) you'll get an error that new certificates are required.

d) Run docker-machine regenerate-certs default and select yes to confirm.

e) Run docker-machine ip default and you'll see the machine is now running on the LAN network. Other machines on the LAN can now access exposed ports on this machine.

4. Checkout the IP of the new bridged adapter

docker-machine ssh default

ifconfig

You'll see the <bridged-IP> on eth1 or similar.

5. Checkout connectivity to the bridged adapter of that docker machine

From a computer in the same network, ping <bridged-IP>.

NOTE: In my case, pinging gave more timeouts than replies. I tried from two PCs, one in the same physical network, the other one at home via VPN. I got the same results, which makes me think the issue is with the VM itself, not the network.
 I found the cause for the above error: the machine has 2 default routes. I deleted the one via 10.0.2.2 and now pings started working! YMMV.

6. Create a swarm on the docker machine

docker swarm init --advertise-addr <bridged-IP>

Now that machine is a swarm manager.

7. Join the node swarm

docker swarm join --token ... 

Copy the command displayed by the previous init command and run it on another machine in the same network. If you don't have that available, you can find it by running this on the swarm manager:

docker swarm join-token worker

8. Create overlay network on the swarm manager

docker network create -d overlay --attachable --subnet 10.1.1.0/24 wan

9. To see the overlay network on swarm workers, you need to actually run a container against it. It will appear automatically...


# Docker Cluster of Containers Across Physical Machines

## Overview

I needed to run a Consul (by HashiCorp) Cluster where some nodes of this cluster are running on a physical machine while other nodes run on another physical machine.

These instructions also apply to any cluster of containers that need to communicate to each other.

Because Docker networks running on different physical machines are by default behind NAT, they cannot inter-communicate directly. So you have three options here:

1) Use public IP of the physical host

2) Interconnect the Docker networks somehow

3) Use Docker Overlay network, which appears automatically on all Docker nodes that are part of the same swarm

Option 1) limits you to one Consul instance per physical machine, which is not very useful.

Option 2) seems more tedious, as you need some IP routing mechanism.

Option 3) is more appealing, as it promises to solve at least part of the problem. And it does, although a some work is still required.

So I chose to create an overlay network, which requires a swarm.
 
Did some research on this, it appears that:

- With Docker for Mac or Windows, you can only create a single-node swarm [1], and this is not useful for our use case

- With Docker for Linux, you can create swarm nodes natively [1]

- If you want to use a Mac or Windows, then you need Docker Machine [1]

Since I currently don't have 2 Linux boxes, I tried with Docker Machine.

The thing with Docker Machine is that it's behind NAT and you cannot access resources outside of localhost/loopback. You could use several docker machines on the same physical host, but that's not a use case I'm interested in. We need to run this on different physical machines.

One option with Docker Machine is to add a bridged network adapter to it, using VirtualBox [2], [3]. I chose this option.

## Steps

In these steps, "boatdirectory" is a sample app I developed exclusively for this project. It's available [here](https://github.com/sorelmitra/learn/tree/master/java/spring/spring-cloud). "Consul" is the app developed by HashiCorp.

### A) Create an Overlay Network between Two Docker Machines with Bridged Network

Create a Docker Overlay Network, with Bridged Docker-Machine. This requires a Docker swarm, even if you don't plan to use the swarm further. See the dedicated section on this above.

### B) Create an overlay network "wan"

See above section.

### C) Start Consul on the swarm worker

Start Consul server 1:

	docker run --name 'c1' -p 8401:8400 -p 8501:8500 -p 8601:8600/udp --ip=10.1.1.11 --net wan -d consul:latest consul agent -dev -client=0.0.0.0 -bind=10.1.1.11

Start Consul agent 2:

	docker run --name 'c2' -p 8402:8400 -p 8502:8500 -p 8602:8600/udp --ip=10.1.1.12 --net wan -d consul:latest consul agent -dev -client=0.0.0.0 -bind=10.1.1.12 -join=10.1.1.11:8301

### D) Start Consul on the swarm manager

Start Consul agent 3:

	docker run --name 'c3' -p 8403:8400 -p 8503:8500 -p 8603:8600/udp --ip=10.1.1.13 --net wan -d consul:latest consul agent -dev -client=0.0.0.0 -bind=10.1.1.13 -join=10.1.1.11:8301

Start Consul agent 4:

	docker run --name 'c4' -p 8404:8400 -p 8504:8500 -p 8604:8600/udp --ip=10.1.1.14 --net wan -d consul:latest consul agent -dev -client=0.0.0.0 -bind=10.1.1.14 -join=10.1.1.11:8301

### E) Start boat directory on the swarm worker

Start boat directory server:

	docker run --name 'bd' --ip=10.1.1.21 -p 8770:8770 --net wan -d boatdirectory:0.1

Start boat directory client:

	docker run --name 'bdc' --ip=10.1.1.22 -p 8080:8080 --net wan -d boatdirectory-client:0.1

### F) Enjoy your Consul Cluster

Access the Consul UI on any of the nodes, using http://<Docker Machine IP>:<Consul Mapped Port>/ui, where <Docker Machine IP> is the IP as displayed by the command "docker-machine env default", while <Consul Mapped Port> is the port 8500 as mapped in the "docker run" command.

## References

[1] https://docs.docker.com/engine/swarm/swarm-tutorial/#use-docker-for-mac-or-docker-for-windows
[2] https://forums.docker.com/t/docker-swarm-on-2-physical-macbooks/36448/3
[3] https://github.com/docker/machine/issues/1491#issuecomment-172325129
