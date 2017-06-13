- If variable is used before it is defined, there should be an error.
  - The whole `assign_homes` pass needs some work. Should we run this
    pass on the QBE ADT instead of the pseudo-x86 ADT?
  - `assign_homes` currently allocates 8 bytes for all data. W, S can
    be allocated only 4 bytes(note: padding will need to be added for
    stack alignment)
- Register allocation. Currently, everything gets spilled to memory.
- Currently, no typechecking happens. Typechecking for QBE IR should
  be fairly straightforward to implement: for a function check that
  returned value matches the function prototype; for each instruction,
  check that the arguments being passed typechecks.
- Strings are not handled.
- `type` definitions are currently not implemented.
- If a function is called that is not defined, assume it is an
  `extern`(?)
- Conversion out of SSA needs to be better. Currently, it naively
  inserts `copy`s to predecessors. This may lead to many copy
  instructions. 

  There are other concerns here too-- see Torczon & Cooper section
  9.3.5

- Build a testing setup similar to QBE's. A .ssa file will contain QBE
  functions along with a comment that specifies the driver(in C) and
  what output it expects. When running the test suite, the test
  program gets linked to the driver and compared against the specified
  output.
