FROM ocaml/opam:ubuntu

RUN opam update && opam upgrade

RUN opam switch create 4.12.0 && eval $(opam env)

WORKDIR /home/opam

USER root

# Install dependencies
RUN apt install -y llvm-14 cmake clang && \
    opam install menhir ppx_deriving llvm.14.0.6

USER opam

COPY --chown=opam . .

RUN make
