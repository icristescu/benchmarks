Build and run this image

```
docker build . -t tezos_test
docker run --mount type=bind,source=<path-to-archive-store>,target=/data tezos_test ./tezos_script.sh
```

where `<path-to-archive-store>` should be a folder containing `archive_store` and the `blocks_above_933913` file. Note that the `archive_store` will be modified by the benchmarks, so be sure that it is a copy of the initial archive store to benchmark (which you can get from `/data/ioana/init_archive_store/` on `comanche`.

On some systems there might be permission issues, so run it with `--user=root`.

When the benchmarks finished, it will generate two files log files at `<path-to-archive-store>`, one for the node and one for the baker. It will also generate two plots `blocks_validated.png` and `obj_added.png`.
