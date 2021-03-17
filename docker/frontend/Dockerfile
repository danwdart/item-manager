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

ENV JSADDLE_WARP_PORT 8080

RUN nix-build default-fix-ghcjs.nix -o result/frontend -A ghcjs.frontend

RUN nix-build -o result/warp -A ghc.wai-app-static

CMD "result/warp/bin/warp" "-p" $JSADDLE_WARP_PORT "-d" "result/frontend/bin/frontend.jsexe"