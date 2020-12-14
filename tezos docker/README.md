### Layered store benchmarks

Build and run this image

```
docker build . -t tezos_test --build-arg tezos_branch=layers_bench_context
docker run --mount type=bind,source=<path-to-archive-store>,target=/data tezos_test ./offline_bootstrap.sh
```

where `<path-to-archive-store>` should be a folder containing `archive_store` and the `blocks_above_933913` file. Note that the `archive_store` will be modified by the benchmarks, so be sure that it is a copy of the initial archive store to benchmark (which you can get from `/data/ioana/init_archive_store/` on `comanche`.

In the `offline_bootstrap.sh` script, `tezos_node` connects to a baker on port `8732`. `tezos-baker-006-PsCARTHA` is a fake baker: it reads file `blocks_above_933913`, which contains already produced blocks on mainnet, and it tries to simulate the operations as it would have done if it was normally producing blocks. So, the baker uses the file to generate the next block with id `933914` and to send it over to the `tezos_node`.
The `baker` sends id `933914` to the node.

Once the `offline_bootsrap.sh` finishes, all blocks that have been read from the file and sent over to the node; they are now in the archive store.

On some systems there might be permission issues, so run it with `--user=root`.

When the benchmarks finished, it will generate two files log files at `<path-to-archive-store>`, one for the node and one for the baker. It will also generate two plots `blocks_validated.png` and `obj_added.png`.

Alternatively you can run a normal bootstrapping for 5 hours
```
docker build . -t tezos_test --build-arg tezos_branch=vbot@store_layer
docker run --mount type=bind,source=<path-to-data>,target=/data tezos_test ./bootstrap.sh
```
where <path-to-data> is the folder where the logs and the resulting plots will be stored.

The `init_archive_store` contains the store with blocks upto `933913` and is used to initalize a store for the baker to read from. Once the `offline_script.sh` finishes executing it has read all the block that are to be read.
