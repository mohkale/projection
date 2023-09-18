ARG EMACS_VERSION=master
FROM silex/emacs:$EMACS_VERSION-ci-cask

ARG UNAME=projection-docker

# Setup basic git credentials so tests that use git work.
RUN git config --global user.email "$UNAME@nowhere.com" \
 && git config --global user.name "$UNAME"

# # Install all Emacs package dependencies.
# RUN mkdir -p /tmp/projection-build
# COPY Cask /tmp/projection-build
# COPY src /tmp/projection-build/src
# RUN cd /tmp/projection-build \
#  && cask install \
#  && rm -rvf /tmp/projection-build

# Install all build/test dependencies.
RUN apt-get update \
 && apt-get install -y software-properties-common wget \
 && wget -O - https://apt.kitware.com/keys/kitware-archive-latest.asc 2>/dev/null \
  | gpg --dearmor - \
  | tee /etc/apt/trusted.gpg.d/kitware.gpg >/dev/null \
 && apt-add-repository "deb https://apt.kitware.com/ubuntu/ focal main" \
 && apt-get update \
 && apt-get install -y cmake npm yarn ninja-build golang \
 && apt-get clean \
 && npm install -g yarn \
 && rm -rf /var/lib/apt/lists/*

ENV PATH=$PATH:/nix/store/emacs/bin/:$HOME/.cask/bin/

WORKDIR /workarea
CMD bash
