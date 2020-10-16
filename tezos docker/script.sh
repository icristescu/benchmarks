#! /usr/bin/env bash

docker run --name tezos_test --mount type=bind,source=../tezos_docs/archive_store_docker,target=/data -c "./tezos-node run --singleprocess --rpc-addr :8732 --no-bootstrap-peers --connections 0 --data-dir /data/archive_store_docker & ./tezos-baker-006-PsCARTHA run with local node /data/archive_store /data/blocks_above_933913"
