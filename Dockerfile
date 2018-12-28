FROM ubuntu:18.04

RUN apt-get update && apt-get install -y libgmp10 build-essential zlib1g-dev libpq-dev ca-certificates

# Copy config, static, and the executable
COPY config /config
COPY static /static

# Must run stack --docker build --copy-bins before this will be available
COPY .stack-work/docker/_home/.local/bin/website /opt/website
RUN echo "User: \n"
RUN echo $YESOD_PGUSER
RUN echo "Database: \n"
RUN echo $YESOD_PGDATABASE
RUN echo $PORT

CMD ["/opt/website"]
