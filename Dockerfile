ARG EMACS_VERSION=master
FROM silex/emacs:$EMACS_VERSION-ci-cask

ARG UNAME=projection-docker

# Setup basic git credentials so tests that use git work.
COPY docker/gitconfig /root/.gitconfig

# # Install all Emacs package dependencies.
# RUN mkdir -p /tmp/projection-build
# COPY Cask /tmp/projection-build
# COPY src /tmp/projection-build/src
# RUN cd /tmp/projection-build \
#  && cask install \
#  && rm -rvf /tmp/projection-build

RUN chmod 755 -R /root/ \
 && chmod 777 -R /root/.cask \
 && mkdir -p /nonexistent \
 && chown -R nobody /nonexistent
COPY docker/gitconfig /etc/gitconfig

# Install all build/test dependencies.
RUN apt-get update \
 && apt-get install -y wget \
 && wget -O - https://apt.kitware.com/keys/kitware-archive-latest.asc 2>/dev/null \
  | gpg --dearmor - \
  | tee /etc/apt/trusted.gpg.d/kitware.gpg >/dev/null \
 && echo "deb https://apt.kitware.com/ubuntu/ jammy-rc main" >> /etc/apt/sources.list \
 && apt-get update \
 && apt-get install -y cmake npm yarnpkg ninja-build golang meson gradle python3-pytest \
 && apt-get clean \
 && npm install -g yarn \
 && rm -rf /var/lib/apt/lists/*

ENV PATH=$PATH:/nix/store/emacs/bin/:/root/.cask/bin:$HOME/.cask/bin/

CMD bash
