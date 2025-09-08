This example showcases the coverage of Rust programs.

To run the whole example, just run `make`. It will compile the project with
instrumentation, run the executable, and create a gnatcoverage report from the
generated trace file.

Additionally, variables can be passed to `make`:

- `make MCDC=1`: Enable MC/DC coverage (is simple statement coverage otherwise)
- `make DEBUG=1`: Enable debug build
