FROM fpco/stack-build:lts-14.7

COPY . /app
WORKDIR /app

RUN stack install --ghc-options -O2

CMD telebot