FROM debian:bullseye-slim

LABEL maintainer="Shintaro Mukai"

ARG PAREN_HOME=/usr/local/bin/paren
ENV PATH=$PAREN_HOME:$PATH

COPY . $PAREN_HOME

RUN apt update \
  && apt -y upgrade \
  && apt -y install build-essential \
  && cd $PAREN_HOME/src \
  && make os=unix clean \
  && make os=unix cc=gcc debug=on

WORKDIR ~/
