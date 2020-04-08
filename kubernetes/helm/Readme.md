# Introduction

Trying to learn about Helm.



# Resources

[1] https://helm.sh/docs/



# Lecture Notes

## Concepts

*Chart*: Bundle of information necessary to create an instance of a Kubernetes application.  Think of it like the Kubernetes equivalent of a Homebrew formula, an Apt dpkg, or a Yum RPM file.

*Config*: Contains configuration information that can be merged into a packaged chart to create a releasable object.

*Repository*: The place where charts can be collected and shared. It’s like Perl’s CPAN archive or the Fedora Package Database, but for Kubernetes packages.

*Hub*: A collection of helm _charts_ from dozens of different _repositories_.

*Release*: A running instance of a chart, combined with a specific config.  One chart can often be installed many times into the same cluster. And each time it is installed, a new release is created. Consider a MySQL chart. If you want two databases running in your cluster, you can install that chart twice. Each one will have its own release, which will in turn have its own release name.

*Library*: Interfaces with Kubernetes API Server to execute the following operations:
- Combining a chart and configuration to build a release
- Installing charts into Kubernetes, and providing the subsequent release object
- Upgrading and uninstalling charts by interacting with Kubernetes

*Client*: End-users CLI, used for:
- Local chart development
- Managing repositories
- Managing releases
- Interfacing with the Helm _library_

*Helm*: We can define Helm as a tool for managing Kubernetes packages called _charts_.  Helm installs _charts_ into Kubernetes, creating a new _release_ for each installation. And to find new _charts_, you can search Helm chart _repositories_.

## Directory Structure

*Chart.yaml*: A YAML file containing information about the chart.

*LICENSE*: OPTIONAL: A plain text file containing the license for the chart.

*README.md*: OPTIONAL: A human-readable README file.

*values.yaml*: The default configuration values for this chart.

*values.schema.json*: OPTIONAL: A JSON Schema for imposing a structure on the values.yaml file.

*charts/*: A directory containing any charts upon which this chart depends.

*crds/*: Custom Resource Definitions.

*templates/*: A directory of templates that, when combined with values, will generate valid Kubernetes manifest files.

*templates/NOTES.txt*: OPTIONAL: A plain text file containing short usage notes.
