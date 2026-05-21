FROM haskell:9.8

RUN apt-get update && apt-get install -y \
    git \
    vim \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /workspace

RUN cabal update

CMD ["bash"]