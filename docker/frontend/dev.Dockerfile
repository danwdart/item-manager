FROM nixos/nix

WORKDIR /app

COPY docker/config/nix.conf /etc/nix/nix.conf

RUN nix-channel --update
RUN nix-env -u
RUN nix-env -iA nixpkgs.cachix nixpkgs.git
RUN cachix use websites
RUN nix-collect-garbage -d

COPY . .

RUN git submodule update --init --recursive

RUN nix-build -A shells.ghc

RUN ./scripts/build-cabal.sh

CMD ["./scripts/ghcid-frontend.sh"]