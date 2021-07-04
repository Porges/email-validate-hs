# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.187.0/containers/ubuntu/.devcontainer/base.Dockerfile

# [Choice] Ubuntu version: bionic, focal
ARG VARIANT="focal"
FROM mcr.microsoft.com/vscode/devcontainers/base:0-${VARIANT}

RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev libtinfo-dev

RUN curl -sSL https://get.haskellstack.org/ | sh
