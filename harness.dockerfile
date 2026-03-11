FROM ubuntu:22.04

ENV PETR4_DEPS="pkg-config \
               sudo \
               git \
               m4 \
               vim \
               libgmp-dev \
               opam \
               ca-certificates \
               curl \
               unzip"

ENV DEBIAN_FRONTEND=noninteractive

COPY . /petr4/
WORKDIR /petr4/

RUN apt-get update && \
    apt-get install -y --no-install-recommends $PETR4_DEPS

RUN opam init --disable-sandboxing -y && \
    eval $(opam env) && \
    opam switch create petr4 4.14.0 && \
    opam init --disable-sandboxing -y && \
    eval $(opam env) && \
    opam switch import petr4-013.export -y && \
    dune build && \
    dune install

RUN tar xvzf testdata/ebpf-tests.tar.gz -C testdata && \
    tar xvzf testdata/v1model-tests.tar.gz -C testdata && \
    tar xvzf testdata/typecheck.tar.gz -C testdata

COPY entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]

CMD ["bash"]
