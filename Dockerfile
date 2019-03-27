FROM lnl7/nix:2.0

WORKDIR /tmp/build

COPY . /tmp/build

# Install tools needed by builtins.fetchTarball, and then install all
# dependencies into its own layer, which doesn't change.
RUN nix-env -f '<nixpkgs>' -i gnutar gzip && \
    nix-shell -Q -j2 --run true

RUN bash -xe build.sh
