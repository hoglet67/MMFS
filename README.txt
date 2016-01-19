MMFS - a modern SD Card solution for 8-bit Acorn Computers
==========================================================

MMFS is one of several ROMs that allow modern SD Cards to be used on a
Beeb with minimal external hardware (others include the original
SuperMMC, Steve Picton's TurboMMC and Duikkie's SmartSPI). MMFS was
written by Martin Mather in 2012, but seems to have been forgotten
about until it was rediscovered in 2015.

MMFS has the following features:

- it's built from simple text source files (BeebASM), rather than
  patching existing binaries with BBC Basic programs

- it's derived from Acorn DFS 2.x rather than Acorn DFS 0.9

- the Master and Beeb/Electron SWRAM versions result in PAGE at &E00

- it preserves the current drive on BREAK

- it supports FAT32 (upto 8GB) as well as FAT16

- I'm motivated to fix any bugs that people find!

Building MMFS is currently only supported on Linux. All dependent
tools (BeebASM 1.04 and mmb_utils) are included in the tools/
directory.

To build, run the build.sh script. The results will be in the build/
directory.

To do a release build, run the release.sh script. The results will be
in the releases/ directory.

The following versions are created:
- Beeb ROM                             (MMFS)
- Beeb ROM for the TurboMMC hardware   (MMFST)
- Beeb Sideways RAM                  (SWMMFS)
- Electron ROM                        (EMMFS)
- Electron Sideways RAM             (ESWMMFS)
- Master                             (MAMMFS)
- Master for the TurboMMC hardware   (MAMMFST)

Original Interface Harware
==========================

This is the original interface design, as conceived by Martin Mather
in 2006.

A schematic of the original hardware can be found here:
http://swhs.home.xs4all.nl/bbc/mmbeeb/

The original hardware uses the following BBC User Port connections:

User Port     SD Card
 (Master)     (Slave)
=========     =======
  CB1/PB1 ==> S_CLK  (Clock)
      CB2 <== S_MISO (Dout)
      PB0 ==> S_MOSI (Din)
       0V ==> S_SEL  (Select)

TurboMMC Interface Hardware
===========================

A schematic of the TurboMMC hardware can be found here:
http://stardot.org.uk/forums/viewtopic.php?t=9445&f=3#p110691

The TurboMMC hardware is a more complicated, as it uses PB[4:2] to
control three buffers, providing two different operating modes.

When PB[4:2] are 010, then the hardware is configured as above, and
the interface is compatible with the original hardware.

When PB[4:2] are 101, then the hardware is re-configured as: 

User Port     SD Card
 (Master)     (Slave)
=========     =======
  CB1/PB1 ==> S_CLK  (Clock)
      n/c <== S_MISO (Dout)
      CB2 ==> S_MOSI (Din)
       0V ==> S_SEL  (Select)

Currently this second mode (which would support fast writes using
6522 Shift Register Mode 6), is not used by MMFS.
