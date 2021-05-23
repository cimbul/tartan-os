FROM rust:slim
RUN rustup toolchain install nightly --allow-downgrade --component rust-src,rustfmt,clippy
WORKDIR /work
