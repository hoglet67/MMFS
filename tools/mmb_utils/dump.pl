#!/usr/bin/perl -w
use strict;

# Beeb Utilities to manipulate MMB and SSD files
# Copyright (C) 2012 Stephen Harris
# 
# See file "COPYING" for GPLv2 licensing

use FindBin;
use lib "$FindBin::Bin";
use BeebUtils;

use FileHandle;

# *DUMP equivalent

my $bytes_per_line=8;
my $addrsize=4;
if (@ARGV && $ARGV[0] eq '-w')
{
  $bytes_per_line=16;
  $addrsize=8;
  shift @ARGV;
}

my $file=$ARGV[0];
die "Syntax: $BeebUtils::PROG [-w] filename\n" unless $file;

my $f=new FileHandle "<$file";
die "Can not open $file: $!\n" unless $f;
binmode($f);

my $buffer;
my $offset=0;
while (sysread($f,$buffer,$bytes_per_line))
{
  my @d=map { ord($_) } split(//,$buffer);
  my ($hex,$ascii);
  foreach (0..$bytes_per_line-1)
  {
    my ($nhex,$nch);
    my $ch=$d[$_];
    if (!defined($ch))
    {
      $nhex="  "; $nch=" ";
    }
    else
    {
      $nhex=sprintf("%02X",$ch);
      if ($ch < 32 || $ch > 126) { $nch="."; } else { $nch=chr($ch); }
    }
    $hex .= "$nhex ";
    $ascii .= $nch;
  }
  printf "%0${addrsize}X %s %s\n",$offset,$hex,$ascii;
  $offset += $bytes_per_line;
}
