FROM ubuntu:22.04

RUN apt update \
  && apt install -y --no-install-recommends \
    ocaml \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

ARG USER
ARG GROUP

RUN groupadd ${USER} \
  && useradd ${USER} -g ${GROUP} -m

USER ${USER}

RUN mkdir /home/${USER}/work

WORKDIR /home/${USER}/work
