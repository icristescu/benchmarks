Build and run this image

```
docker build . -t tezos_test
docker run --mount type=bind,source=<path-to-archive-store>,target=/data tezos_test ./tezos-script.sh
```

where <path-to-archive-store> should be a folder containing `archive_store` and the `blocks_above_933913` file.
