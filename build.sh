#!/bin/bash

stack install --docker --fast
docker build -t website:base .

images=$(echo $IMAGES | tr " " "\n")

for image in $images
do
    docker tag website:base $image

    if $PUSH_IMAGE
    then
        docker push $image
    fi
done
