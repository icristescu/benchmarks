#! /usr/bin/env bash

./tezos-node run --singleprocess --rpc-addr :8732 --no-bootstrap-peers --connections 0 --data-dir /data/archive_store >> /data/node_logs 2>> /data/node_err & sleep 5; ./tezos-baker-006-PsCARTHA run with local node /data/archive_store /data/blocks_above_933913 >> /data/baker_logs 2>> /data/baker_err
