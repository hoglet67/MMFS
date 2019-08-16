MMFS - a modern SD Card solution for 8-bit Acorn Computers
==========================================================

MMFS is one of several ROMs that allow modern SD Cards to be used on a
Beeb with minimal external hardware (others include the original
SuperMMC, Steve Picton's TurboMMC and Duikkie's SmartSPI). MMFS was
written by Martin Mather in 2012, but seems to have been forgotten
about until it was rediscovered in 2015.

For more details on the release structures, see:
    https://github.com/hoglet67/MMFS/wiki/Release-structure

For binary releases, see:
    https://github.com/hoglet67/MMFS/releases

For further documention see:
    https://github.com/hoglet67/MMFS/wiki

MMFS has the following features:

- it's built from simple text source files (BeebASM), rather than
  patching existing binaries with BBC Basic programs

- it's derived from Acorn DFS 2.x rather than Acorn DFS 0.9

- the Master and Beeb/Electron SWRAM versions result in PAGE at &E00

- it preserves the current drive on BREAK

- it supports FAT32 (upto 8GB) as well as FAT16

- I'm motivated to fix any bugs that people find!

Building MMFS is supported on Linux and MacOS. All dependent
tools (BeebASM and mmb_utils) are included in the tools/
directory.

To build, run the build.sh script. The results will be in the build/
directory.

To do a release build, run the release.sh script. The results will be
in the releases/ directory.


Original Interface Hardware
===========================

This is the original interface design, as conceived by Martin Mather
in 2006.

A schematic of the original hardware can be found here:
https://web.archive.org/web/20171002181904/https://swhs.home.xs4all.nl/bbc/mmbeeb

The original hardware uses the following BBC User Port connections:

User Port     SD Card
 (Master)     (Slave)
=========     =======
  CB1/PB1 ==> S_CLK  (Clock)
      CB2 <== S_MISO (Dout)
      PB0 ==> S_MOSI (Din)
       0V ==> S_SEL  (Select)

With this hardware, the 6522 shift register mode 2 can be used to
provide high speed reads ("turbo reads"). This feature was introduced
in MMFS 1.13.

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

With this hardware, the 6522 shift register modes 2 and 6 can be used
to provide high speed reads ("turbo reads") and high speed writes
("turbo writes") respectively. This feature was introduced in MMFS
1.15.
