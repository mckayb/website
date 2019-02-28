# Structured Rants

[![CircleCI](https://circleci.com/gh/mckayb/website.svg?style=svg)](https://circleci.com/gh/mckayb/website)

## Running Kubernetes Cluster Locally
Install the following:
  * [Docker](https://docs.docker.com/docker-for-mac/install/)
  * [Helm](https://www.helm.sh/)
  * [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)

Once you've [enabled kubernetes](https://docs.docker.com/docker-for-mac/#kubernetes), initialize helm by running
```
helm init
```
Follow the steps defined [here](https://kubernetes.github.io/ingress-nginx/deploy/) to install the ingress controller. (Mandatory steps, as well as the helm steps will work fine.)

Next, pull the base image that you'll use to build the repo with the following:
```
docker pull fpco/stack-build:lts-13.9
```
The tag `lts-13.9` should match the resolver in the `stack.yaml`, so adjust accordingly. Be aware, this will take a while... go get a coffee or something.

Build the application image with:
```
stack --docker setup
stack --docker install
docker build -t website:v1 .
```
The first time running the install will also take a while.

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
Replace the url with whatever you used for the `ingress.hosts` value.
```
127.0.0.1 structuredrants.local
```

Build the helm dependencies with:
```
helm dependencies update charts/website
```

Run the application with:
```
helm install charts/website
```

Feel free to check the status of the deployment with
```
kubectl get pods -w
```

Once all the pods are in a ready state, use the application by visiting the domain you used in the `ingress.hosts` value in your browser.
```
http://structuredrants.local
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
