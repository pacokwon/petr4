#!/bin/bash
# Load the opam environment
eval $(opam env)

# Execute the CMD passed from Docker (e.g., /bin/bash or dune build)
exec "$@"
