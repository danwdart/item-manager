# item-manager

Manages items. Very generic.

Shows a CRUD view. You can edit items and categories.

Responsive for use on phones or phone emulating in Chrome.

Faster builds are on Docker as the images have already been pushed so Docker is recommended but Nix is here just in case.

## Installation

### Installation using Nix

Add Ryan Trinkle's cache for the Reflex libraries:
https://github.com/reflex-frp/reflex-platform/blob/develop/notes/NixOS.md

Add my own cache for everything else:
```
nix-env -iA nixpkgs.cachix
cachix use websites
```

Then build:

`scripts/build.sh`

Running the backend:
1. Run the backend app: `result/backend/bin/backend`
1. Visit `http://localhost:8081`.

Running the frontend:
1. Serve the frontend app: `result/warp/bin/warp -p 8080 -d result/frontend/bin/frontend.jsexe`
1. Visit `http://localhost:8080`.

### Installation using Docker

`docker-compose -f docker-compose.yml up -d` should run everything.
`docker-compose -f docker-compose.yml build` should make everything from scratch.

## Development

### Development using Nix

Add Ryan Trinkle's cache for the Reflex libraries:
https://github.com/reflex-frp/reflex-platform/blob/develop/notes/NixOS.md

Add my own cache for everything else:
```
nix-env -iA nixpkgs.cachix
cachix use websites
```

Build first:

`scripts/build.sh`

Then use these to run the development auto-reloading version:

1. `scripts/ghcid-frontend.sh` to run the frontend on port 8080 (currently Chrome only due to the realtime rendering).
1. `scripts/ghcid-backend.sh` to run the backend on port 8081.

### Development using Docker

`docker-compose -f docker-compose.yml up -d` should run everything.
`docker-compose -f docker-compose.yml build` should make everything from scratch.

## Testing

### Testing using Nix

Assuming the application was built, you can simply run:

`scripts/test.sh`.

You can also live-test parts of the app, like:

`scripts/test-watch frontend`

`scripts/test-watch backend`

`scripts/test-watch common`

### Testing using Docker

At the moment, the best way is probably to just run the tests from one of the apps:

Either:

`docker-compose -f docker-compose.yml run --rm backend scripts/test.sh`

if you installed as in "Installation", or:

`docker-compose run --rm backend scripts/test.sh`

if you installed as in "Development".