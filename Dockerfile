ARG VERSION
FROM silex/emacs:$VERSION-ci-cask

ARG UNAME=projection

RUN git config --global user.email "$UNAME@docker.com" \
 && git config --global user.name "$UNAME"

WORKDIR /workarea
CMD bash
