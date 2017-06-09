- If variable is used before it is defined, there should be an error.
  - The whole `assign_homes` pass needs some work. Should we run this
    pass on the QBE ADT instead of the pseudo-x86 ADT?
- Handle floating point numbers and operations
  - This is partially done now(except ordered/unordered comparison
    operators-- cod, cos, cuod, cuos), but currently floating-point
    ops are carried out using FPU. We really should be using SSE
    instead.
  - Floats should get hoisted out into `data` segment. Currently, this
    needs to be done manually in the QBE program
- `type` definitions are currently not implemented.
- If a function is called that is not defined, assume it is an
  `extern`(?)
