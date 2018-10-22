FROM ubuntu:18.04

RUN apt-get update && apt-get install -y libgmp10 build-essential zlib1g-dev libpq-dev

