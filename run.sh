#!/bin/bash

SCALPEL=$(find backend/dist-newstyle/build -type f -name "my-scalpel")

if [[ -z $SCALPEL ]]; then
  echo "must build backend. Run command below:"
  echo "cd backend; cabal build;"
  exit 1
fi

if [[ $1 == "scrape" ]]; then
  $SCALPEL persist --config config.yaml --data data --db backend/scalpel.db
  exit 0
fi

SERVER=$(find backend/dist-newstyle/build -type f -name "my-scalpel-server")

if [[ $1 == "serve" ]]; then
  $SERVER --db backend/scalpel.db
  exit 0
fi

echo "Provide command: scrape, serve"
