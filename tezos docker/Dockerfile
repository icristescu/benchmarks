FROM ocurrent/opam:debian-10-ocaml-4.09

RUN sudo apt-get update && sudo apt-get install -qq -yy libffi-dev \
        liblmdb-dev m4 pkg-config gnuplot-x11 libgmp-dev libhidapi-dev

RUN git clone https://github.com/icristescu/tezos.git

COPY --chown=opam:opam ["tezos.export", "tezos/tezos.export"]

COPY --chown=opam:opam ["tezos_script.sh", "tezos/tezos_script.sh"]

WORKDIR tezos

RUN git checkout layers_bench_context

RUN opam update

RUN opam switch 4.09

RUN opam switch import tezos.export

RUN opam pin add index.dev https://github.com/icristescu/index.git#throttle_log -n

RUN opam pin add ppx_irmin.dev https://github.com/icristescu/irmin.git#hashtbl_upper -n

RUN opam pin add irmin.dev https://github.com/icristescu/irmin.git#hashtbl_upper -n

RUN opam pin add irmin-layers.dev https://github.com/icristescu/irmin.git#hashtbl_upper -n

RUN opam pin add irmin-pack.dev https://github.com/icristescu/irmin.git#hashtbl_upper -n

RUN opam install index ppx_irmin irmin irmin-layers irmin-pack

ADD --chown=opam [".", "."]

RUN eval $(opam env)

RUN opam config exec -- make




