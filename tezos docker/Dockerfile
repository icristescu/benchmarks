FROM ocurrent/opam:debian-10-ocaml-4.09

RUN sudo apt-get update && sudo apt-get install -qq -yy libffi-dev \
        liblmdb-dev m4 pkg-config gnuplot-x11 libgmp-dev libhidapi-dev \
        python python3-pip procps

RUN python3 -m pip install matplotlib

RUN python3 -m pip install pandas

ARG tezos_branch

RUN git clone https://github.com/icristescu/tezos.git

COPY --chown=opam:opam ["tezos.export", "tezos/tezos.export"]

COPY --chown=opam:opam ["offline_bootstrap.sh", "tezos/offline_bootstrap.sh"]

COPY --chown=opam:opam ["bootstrap.sh", "tezos/bootstrap.sh"]

COPY --chown=opam:opam ["plot.py", "work/plot.py"]

COPY --chown=opam:opam ["text_extract.ml", "work/text_extract.ml"]

COPY --chown=opam:opam ["dune", "work/dune"]

WORKDIR tezos

RUN git checkout $tezos_branch

RUN opam switch 4.09

RUN opam update

RUN eval $(opam env)

RUN opam switch import tezos.export

RUN opam pin add repr.dev https://github.com/mirage/repr.git#main

RUN opam pin add ppx_repr.dev https://github.com/mirage/repr.git#main

RUN opam pin add index.dev https://github.com/mirage/index.git#master

RUN opam pin add ppx_irmin.dev https://github.com/mirage/irmin.git#master --ignore-pin-depends

RUN opam pin add irmin.dev https://github.com/mirage/irmin.git#master --ignore-pin-depends

RUN opam pin add irmin-layers.dev https://github.com/mirage/irmin.git#master --ignore-pin-depends

RUN opam pin add irmin-pack.dev https://github.com/mirage/irmin.git#master --ignore-pin-depends

RUN opam reinstall repr ppx_repr index irmin irmin-layers irmin-pack

ADD --chown=opam [".", "."]

RUN eval $(opam env)

RUN opam config exec -- make
