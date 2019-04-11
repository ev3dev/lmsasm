% LMSASM(1) | User's Manual
% David Lechner
% October 2016

# NAME

lmsasm - LEGO MINDSTORMS bytecode assembler

# SYNOPSIS

lmsasm [--support *vm*] [--version *version*] [--debug] [--ev3g] [--output *out-file*] *in-file*

# DESCRIPTION

Compiles an lms bytecode file to lms2012 VM machine language.

# OPTIONS

--support *vm*
: The target VM bytecode definitions. This can be "official" for targeting the
official LEGO firmware/VM, "xtended" for the RoboMatter/National Instruments
extended firmware/VM or "comapt" for the ev3dev `lms2012-comapt` VM. The default
is "official".

--version *version*
: Sets the bytecode version in the header of the output file. Default is 0.
  This doesn't really do anything useful unless you are trying to make the
  output exactly match an existing file byte-for-byte.

--debug
: Useful for debugging compiler problems. Produces lots of output.

--ev3g
: When set, this enables EV3-G compatibility quirks. This causes the generated
  byte codes to match the format used by the official EV3 desktop software.
  This doesn't really do anything useful unless you are trying to make the
  output exactly match an existing file byte-for-byte.

--output *out-file*
: The name of the output file. The default is "out.rbf".

*in-file*
: The name of the input lms bytecode file.
