# Structured Rants

## Status
[![Build Status](https://travis-ci.org/mckayb/website.svg?branch=master)](https://travis-ci.org/mckayb/website)

## Running Kubernetes Cluster Locally
Install the following:
  * [Docker](https://www.docker.com/get-started)
  * [Virtual Box](https://www.virtualbox.org/)
  * [Minikube](https://github.com/kubernetes/minikube)
  * [Helm](https://www.helm.sh/)
  * [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)

Once you have those installed, start up minikube with:
```
minikube start --disk-size=40g
```

Set your docker environment to the one inside minikube with:
```
eval $(minikube docker-env)
```

Build the base image for the application with:
```
docker build -t website:base .
```

Pull the image used to build the image with the following command.
The tag `lts-12.16` should match the resolver in the `stack.yaml`, so adjust accordingly.
```
docker pull fpco/stack-build:lts-12.16
```

Build the application image with:
```
stack --docker build
stack --docker image container
```

Edit the helm values file with:
```
cp charts/website/values.example.yaml charts/website/values.yaml
```
Edit the `charts/website/values.yaml` file with whatever
credentials you see fit. Specifically the following:
  * `ingress.hosts`
  * `admin.email`
  * `admin.password`
  * `postgresql.postgresqlUsername`
  * `postgresql.postgresqlPassword`
  * `postgresql.postgresqlDatabase`

Add the following line into your `/etc/hosts` file.
```
192.168.99.100 chart-example.local
```
Replace `192.168.99.100` with the result of running `minikube ip`. Replace `chart-example.local` with whatever you used for the `ingress.hosts` value.

Run the application with:
```
helm dependencies update charts/website
helm install charts/website
```

Then, use the application by going to whatever domain you used in the `ingress.hosts` value.
```
http://chart-example.local
```

## Running Tests
For the tests to work, you'll need a local installation of
[Postgres](https://www.postgresql.org/).

After installing Postgres, run:

```
createuser homestead --password secret --superuser
createdb website
createdb website_test
```

Afterwards, you can run the tests simply by running
```
stack test
```