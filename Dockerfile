FROM fpco/stack-build:lts-14.7 as dependencies

RUN mkdir /opt/build
COPY stack.yaml package.yaml stack.yaml.lock /app/build/
WORKDIR /app/build

RUN apt-get update \
  && apt-get download libgmp10
RUN mv libgmp*.deb libgmp.deb

RUN stack build --system-ghc --dependencies-only

# ---------------------------------------------------------

FROM fpco/stack-build:lts-14.7 as build
COPY --from=dependencies /root/.stack /root/.stack
COPY . /app/build/
WORKDIR /app/build

RUN stack build --system-ghc
RUN mv "$(stack path --local-install-root --system-ghc)/bin" /app/build/bin

# ---------------------------------------------------------

FROM ubuntu:16.04 as app
RUN mkdir -p /app
WORKDIR /app

COPY --from=dependencies /app/build/libgmp.deb /tmp
RUN dpkg -i /tmp/libgmp.deb && rm /tmp/libgmp.deb

COPY --from=build /app/build/bin .
CMD telebot
