#!/usr/bin/perl -w
use strict;

# Beeb Utilities to manipulate MMB and SSD files
# Copyright (C) 2012 Stephen Harris
# 
# See file "COPYING" for GPLv2 licensing

use FileHandle;
use FindBin;
use lib "$FindBin::Bin";
use BeebUtils;

# Stupid program that reads the passed file, converts 0D to 0A
# (converts BBC to Unix)
# (yeah yeah, tr '\015' '\012' but this is more portable)

my $file=$ARGV[0];
die "Syntax: $BeebUtils::PROG filename\n" unless $file;

my $f=new FileHandle "<$file";
die "Can not open $file: $!\n" unless $f;
binmode($f);

my $buffer;
while (sysread($f,$buffer,10000))
{
  $buffer=~tr/\015/\012/;
  print $buffer;
}
