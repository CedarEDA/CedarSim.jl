# Benchmarks

Each `.jl` file in this directory will be run when you run `make` to generate benchmark outputs, which will be placed in `output/`.
Use `make JULIA="julia +cedar"` when running locally, or `make JULIA="julia +cedar-vanilla-a1b2c3d4"` if you've got a specific version you want to benchmark (installable via `juliaup_cedar.sh vanilla a1b2c3d4`, for instance).
