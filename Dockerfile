FROM ubuntu:22.04

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    ocaml \
    rake \
    ruby \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

ARG USER
ARG GROUP

RUN groupadd ${USER} \
  && useradd ${USER} -g ${GROUP} -m

USER ${USER}

RUN mkdir /home/${USER}/work

WORKDIR /home/${USER}/work

ENV IN_CONTAINER=1
ENV USER=${USER}
