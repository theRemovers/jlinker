Atari Jaguar Removers Linker
============================

This package implements a replacement tool of the original Atari Linker (ALN) needed to develop on Atari Jaguar platform.

Build with `make`. Run the ELF regression tests with `make test` (Python 3 is
required for tests only; no m68k toolchain is needed).

ELF input supports big-endian ELF32 m68k relocatable objects and `R_68K_32`
relocations, with either REL or RELA addends. Allocated sections are merged into
text, data and BSS. Non-allocated sections, including debug information, and
their relocations are discarded.

ELF section and common-symbol alignments are preserved during absolute linking.
Contiguous segments (`x`) receive any necessary padding; explicit segment
addresses must satisfy the required alignment. The `-rw` through `-rq` options
do not weaken ELF alignment requirements.

The a.out output format cannot store ELF alignment requirements. Partial links
(`-p` or `-q`) that would lose such requirements are rejected before opening the
output file. Use an absolute link instead. ELF inputs with no alignment
requirement greater than one can still be partially linked to a.out.
