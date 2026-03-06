FROM ubuntu:22.04

ENV PETR4_DEPS pkg-config \
               sudo \
               git \
               m4 \
               vim \
               libgmp-dev \
               opam \
               ca-certificates \
               curl \
               unzip

ENV DEBIAN_FRONTEND=noninteractive

COPY . /petr4/
WORKDIR /petr4/

RUN apt-get update && \
    apt-get install -y --no-install-recommends $PETR4_DEPS

RUN opam init --disable-sandboxing -y && \
    eval $(opam env) && \
    opam switch create 4.14.0 && \
    opam init --disable-sandboxing -y && \
    eval $(opam env) && \
    opam switch import petr4-013.export -y && \
    dune build && \
    dune install

RUN mv /petr4/testdata/ebpf-tests.tar.gz /petr4/testdata/v1model-tests.tar.gz . && \
    tar xvzf ebpf-tests.tar.gz && \
    tar xvzf v1model-tests.tar.gz

COPY entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]

CMD ["bash"]
