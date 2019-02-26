# Structured Rants

## Status
[![CircleCI](https://circleci.com/gh/mckayb/website.svg?style=svg)](https://circleci.com/gh/mckayb/website)

## Running Kubernetes Cluster Locally
Install the following:
  * [Docker](https://www.docker.com/get-started)
  * [Virtual Box](https://www.virtualbox.org/)
  * [Minikube](https://github.com/kubernetes/minikube)
  * [Helm](https://www.helm.sh/)
  * [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)

Once you have those installed, start up minikube.
```
minikube start --disk-size=40g
```

Set your docker environment to the one inside minikube.
```
eval $(minikube docker-env)
```

Pull the image used to build the image with the following command.
The tag `lts-12.16` should match the resolver in the `stack.yaml`, so adjust accordingly.
```
docker pull fpco/stack-build:lts-12.16
```
Be aware, this will take a while... go get a coffee or something.

Build the application image with:
```
stack --docker build --copy-bins
docker build -t website:v1 .
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

If you changed the name of the image when you built it, replace that too under `image.repository` and `image.tag`.

Add the following line into your `/etc/hosts` file.
Replace the ip with the result of running `minikube ip`. Replace the url with whatever you used for the `ingress.hosts` value.
```
192.168.99.100 chart-example.local
```

Build the helm dependencies with:
```
helm dependencies update charts/website
```

Run the application with:
```
helm install charts/website
```

Then, use the application by visiting the domain you used in the `ingress.hosts` value in your browser.
```
http://chart-example.local
```

## Running Tests
For the tests to work, you'll need a local installation of
[Postgres](https://www.postgresql.org/).

After installing Postgres, run the following.
```
createuser homestead --password secret --superuser
createdb website
createdb website_test
```
If you use different credentials, update the `config/test-settings.yml` file with whatever credentials you used.

Afterwards, you can run the tests simply by running
```
stack test
```
