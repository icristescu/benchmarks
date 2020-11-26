#! /usr/bin/env bash

export RUN_ID=$RANDOM
./tezos-node run --singleprocess --rpc-addr :8732 --no-bootstrap-peers --connections 0 --data-dir /data/archive_store > /data/$RUN_ID-"node_logs" 2>&1 & \
# start the baker once the node is running
sleep 60;
./tezos-baker-006-PsCARTHA run with local node /data/archive_store /data/blocks_above_933913 > /data/baker_logs 2>&1 & \

# extract csv files from the logs
sleep 120;
cd ../work;
for i in {1..10}; do
echo -n "loop $i" > /data/loop;
dune exec -- ./text_extract.exe -f /data/$RUN_ID-"node_logs" -s -i $RUN_ID; cp $RUN_ID-"block_validator" /data/.; sleep 60; done &&\

# kill the node after 10 minutes for testing
ps -ef | grep tezos-node | grep -v grep | awk '{print $2}' | xargs kill && \

dune exec -- ./text_extract.exe -f /data/$RUN_ID-"node_logs" -s -i $RUN_ID && \
dune exec -- ./text_extract.exe -f /data/$RUN_ID-"node_logs" -o -i $RUN_ID && \

# plot them and save the images produced in the data folder
python3 plot.py $RUN_ID-"block_validator" $RUN_ID-"adds" &&\
cp blocks_validated.png /data/. &&\
cp obj_added.png /data/. &&\

# copy csv files too, for the jupyter notebook
cp $RUN_ID-"block_validator" /data/. &&\
cp $RUN_ID-"adds" /data/.
