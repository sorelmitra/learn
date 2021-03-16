# Introduction

This is about installing and configuring Prometheus with Grafana in a Kubernetes lab using Helm charts.  I need to do this in order to gather the following metrics:

- RAM, CPU, and IOPS usage: overall and per container

## Test App that Consumes Resources

In order to make sure I gather the metrics correctly, I'm using a test app that can consume CPU and RAM, available in `resource-consumer.yaml`.  Source: https://github.com/wuestkamp/k8s-example-resource-monitoring.

To install it run:

	kubectl apply -f resource-consumer.yaml

I tested it with CPU and it appears in Linux's `top` command.

Consume resources with:

	curl --data "millicores=400&durationSec=300" <cluster IP>:30980/ConsumeCPU
	curl --data "megabytes=400&durationSec=300" <cluster IP>:30980/ConsumeMem

Alternatively, and better! - use `resource-consumer` from [here]().  Consume resources like this:

Take up 1/8 CPU for 10 minutes:

	curl --data "millicores=125&durationSec=600" <cluster IP>:30980/consume-cpu

Note: One replica of Resource Consumer cannot consume more that 1 cpu.

Take up 200M Memory for 5 minutes:

	curl --data "megabytes=200&durationSec=300" <cluster IP>:30980/consume-mem

Note: Request to consume more memory then container limit will be ignored.

Take up 10G of disk for 10 mintutes:

	curl --data "gigabytes=10&durationSec=600&filename=/var/log/foo.log" <cluster IP>:30980/consume-disk

Note: Requests to create files in non-existent directories will be ignored.

## Attempts to Configure Grafana and Prometheus

Installing and configuring Prometheus with Grafana in a Kubernetes lab using Helm charts is a nightmare.  I finally succeeded but logged below my attempts.

### Try 1: Example Configuration

Tried based on the example configuration from here: https://github.com/wuestkamp/k8s-example-resource-monitoring

But the charts are deprecated and fail.

### Try 2: Prometheus Charts

Now trying based on these docs:

- https://github.com/prometheus-community/helm-charts/tree/main/charts/kube-prometheus-stack
- https://github.com/prometheus-operator/prometheus-operator/blob/master/Documentation/user-guides/exposing-prometheus-and-alertmanager.md
- https://prometheus.io/docs/prometheus/latest/getting_started/
- https://prometheus.io/docs/guides/cadvisor/
- https://github.com/google/cadvisor

Add Helm repo:

	helm repo add prometheus-community https://prometheus-community.github.io/helm-charts
	helm repo update

Show configurable values:

	helm show values prometheus-community/kube-prometheus-stack
	helm show values kubernetes/kube-state-metrics
	helm show values prometheus-community/prometheus-node-exporter
	helm show values grafana/grafana

Install:

	helm install prometheus-stack prometheus-community/kube-prometheus-stack

But the chart fails to install in the lab I was using, it cannot download certain images.  By the time I realized the problem was that that lab didn't have access to Internet, I had already moved to try 3 below.

### Try 3: Loki Grafana

Struggling to install and configure Prometheus based on the [loki-grafana chart](https://grafana.github.io/helm-charts/).  Can't get Prometheus to load the configuration.  Can't find relevant documentation.

Finally found [this](https://grafana.com/docs/grafana-cloud/metrics/prometheus/kubernetes/remote_write_helm_prometheus/).  It tells you how to configure `remote_write` for prometheus using Grafana charts...  Along with the [official Prometheus documentation](https://prometheus.io/docs/prometheus/latest/configuration/configuration/#remote_write), I managed to figure out that you need to put all those values under `server`, like this:

	prometheus:
	  enabled: true
	  server:
	    service:
	      type: NodePort
	      nodePort: 32321

This way I finally managed to expose NodePort on Prometheus...  That's a success because I managed to define a configuration bit that works.

I'm installing `loki-stack` like this:

	helm --namespace monitoring upgrade --install loki grafana/loki-stack --values loki-values.yaml

If I try to disable `loki` I get an error.  This bug confirms the chaotic ways around Kubernetes and Helm: https://github.com/grafana/loki/issues/2731.  That bug was closed automatically and nobody cared.

Moving on, I managed to access Prometheus on the exposed port and get a result of scraping itself via the `prometheus_target_interval_length_seconds` expression.


# Usage

## Install

Last update: 2021-03-15.

Check `loki-values.yaml`.  Configure it to suit your needs as below.

Run:

	helm --namespace monitoring upgrade --install loki grafana/loki-stack --values loki-values.yaml

## Uninstall

Run:

	helm --namespace monitoring delete loki

## Configure

### First Metrics

Follow [this guide](https://prometheus.io/docs/prometheus/latest/getting_started/) to configure your first metrics - on Prometheus itself.

### Google cAdvisor

If you want to take a look a CPU and RAM, you need cAdvisor: https://github.com/stefanprodan/dockprom/issues/7

Google [cAdvisor](https://github.com/google/cadvisor) is a tool for monitoring container resource usage and performance.

Install:

	helm repo add ckotzbauer https://ckotzbauer.github.io/helm-charts
	helm --namespace monitoring install cadvisor ckotzbauer/cadvisor

Configure job in Prometheus:

	- job_name: cadvisor
	  scrape_interval: 5s
	  static_configs:
	- targets:
	  - cadvisor:8080

Of course, this has to sit under server -> scrape_configs.

After adding the above configuration in `loki-values.yaml`, reinstall loki-stack, open the Prometheus /graph endpoint and run this query: `machine_cpu_cores`.

More information about stats cAdvisor exposes to Prometheus here:
https://github.com/google/cadvisor/blob/master/docs/storage/prometheus.md

For example to get the CPU load in the past 10 seconds for the specified container, run this query: `container_cpu_load_average_10s{container="name"}`

### Grafana Dashboard

A useful dashboard for Grafana I found [here](https://stefanprodan.com/2016/a-monitoring-solution-for-docker-hosts-containers-and-containerized-services/) and [here](https://github.com/stefanprodan/dockprom), in `grafana/provisioning/dashboards/docker_containers.json`.

I modified it to be more relevant with multiple nodes Kubernetes cluster, do filter out some of the pods and containers, and to add disk usage.  I saved it into `grafana-dashboard.json`.

## Documentation on Metrics and Dashboards

- [Prometheus Queries](https://prometheus.io/docs/prometheus/latest/querying/basics/).
- [Grafana Editor](https://grafana.com/docs/grafana/latest/panels/panel-editor/)
- [Grafana Prometheus Queries](https://grafana.com/docs/grafana/latest/datasources/prometheus/)
- [cAdvisor Metrics for Prometheus](https://github.com/google/cadvisor/blob/master/docs/storage/prometheus.md)
