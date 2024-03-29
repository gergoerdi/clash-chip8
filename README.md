CHIP-8 implemented in Clash
===========================

Originally implemented for RetroChallenge 2018/09. Progress was
documented at https://gergo.erdi.hu/blog/tags/retrochallenge/ This
repository is a rewrite for my book *Retrocomputing with Clash:
Haskell for FPGA Hardware Design* at <https://unsafePerform.IO/retroclash/>.

It targets the [Nexys A7-50T FPGA development board][1], but the only IO it
needs is VGA output and a four-by-four keypad input, so it should port
easily to any other FPGA platform.

## Building into a bitfile

The included `mk` script runs the included Shake rules and creates a
Xilinx bitfile ready to upload on a Nexys A7-50T. First, create a
`build.mk` file that describes your local build environment and your
build target:

```
VIVADO_ROOT=/path/to/vivado/installation
TARGET=nexys-a7-50t
```

Alternatively, if you are using the Vivado toolchain via a wrapper
script (e.g. to run it in Docker), instead of `VIVADO_ROOT`, you can
set `VIVADO` to the wrapper script's name:

```
VIVADO=/path/to/vivado-wrapper
```

The script will be called with the first argument being the Vivado
tool's name, and the rest of the arguments are the arguments to the
tool itself.

Once you have `build.mk`, you can run `mk` and upload to your FPGA
board the `Top.bit` file from the
`_build/nexys-a7-50t/CHIP8/CHIP8.runs/impl_1` directory.

## Building the simulators

There are three simulators included:

* A ["very high-level"][2] simulation that only uses the CPU
  implementation from Clash, and the rest is Haskell.
  
* A logic board simulation that simulates not just the CPU, but also
  the memory elements, including the memory address decoding.
  
* A low-level simulation that runs the whole board in Verilator, and
  interprets the VGA signal output.

Building the simulators is gated by the `sim` flag, which is set by
default. To build the first two simulations, just do a `stack build`. 
To build the low-level simulation, you need to install Verilator from
https://www.veripool.org/wiki/verilator, and set the `verilator` flag
when building; i.e. do `stack build --flag clash-chip8:verilator` (or
the equivalent setting when using Cabal directly).

You can also disable the simulators with `stack build --flag clash-chip8:-sim`,
which can be useful if you only want to build for real hardware and
don't want to install simulation-only dependencies.

[1]: https://reference.digilentinc.com/reference/programmable-logic/nexys-a7/start
[2]: https://gergo.erdi.hu/blog/2018-09-15-very_high-level_simulation_of_a_c_ash_cpu/
