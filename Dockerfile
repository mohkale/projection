ARG EMACS_VERSION=master
FROM silex/emacs:$EMACS_VERSION-ci

ARG UNAME=projection-docker

# Setup basic git credentials so tests that use git work.
RUN git config --global user.email "$UNAME@nowhere.com" \
 && git config --global user.name "$UNAME"

# Install all Emacs package dependencies.
COPY Eask /root/.eask/
COPY src  /root/.eask/src
ENV PATH=/root/.local/bin/:$PATH
RUN apt-get update \
 && apt-get install -y unzip \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* \
 && curl -fsSL https://raw.githubusercontent.com/emacs-eask/cli/master/webinstall/install.sh | sh \
 && eask install-deps -g --dev

WORKDIR /workarea
CMD bash
