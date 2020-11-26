#! /usr/bin/env bash

export RUN_ID=`date +"%Y-%b-%d-%H-%M-%S"`;
./tezos-node run --singleprocess --connections 3 --data-dir ./data > /data/$RUN_ID-"node_logs" 2>&1 & \

sleep 18000;
ps -ef | grep tezos-node | grep -v grep | awk '{print $2}' | xargs kill && \

# extract csv files from the logs
cd ../work; dune exec -- ./text_extract.exe -f /data/$RUN_ID-"node_logs" -s -i $RUN_ID && \
dune exec -- ./text_extract.exe -f /data/$RUN_ID-"node_logs" -o -i $RUN_ID && \

# plot them and save the images produced in the data folder
python3 plot.py $RUN_ID-"block_validator" $RUN_ID-"adds" &&\
cp blocks_validated.png /data/. &&\
cp obj_added.png /data/. &&\

# copy csv files too, for the jupyter notebook
cp $RUN_ID-"block_validator" /data/. &&\
cp $RUN_ID-"adds" /data/.
