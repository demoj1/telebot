FROM fpco/stack-build:lts-14.7

COPY . /app
WORKDIR /app

RUN stack install -j1 --ghc-options -O2 +RTS -M1500M

CMD telebot