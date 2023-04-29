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

WORKDIR /workarea
CMD bash
