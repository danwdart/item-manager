FROM nixos/nix

WORKDIR /app

RUN nix-channel --update
RUN nix-env -u
RUN nix-env -iA nixpkgs.cachix nixpkgs.git
RUN cachix use websites
RUN nix-collect-garbage -d

COPY docker/config/nix.conf /etc/nix/nix.conf

COPY . .

RUN git submodule update --init --recursive

RUN nix-build -o result/backend -A ghc.backend

CMD [ "result/backend/bin/backend" ]