#!/usr/bin/perl -w
use strict;

# Beeb Utilities to manipulate MMB and SSD files
# Copyright (C) 2012 Stephen Harris
# 
# See file "COPYING" for GPLv2 licensing

use FindBin;
use lib "$FindBin::Bin";
use BeebUtils;

@ARGV=BeebUtils::init(@ARGV);

my %disk=BeebUtils::load_dcat();

foreach (sort {$a <=> $b} keys %disk)
{
  next unless $disk{$_}{Formatted};
  my $d="$_"; $d=" $d" if length($d)==1; $d=" $d" if length($d)==2;
  my $L=$disk{$_}{ReadOnly}?" (L)":"";
  print "$d: $disk{$_}{DiskTitle}$L\n";
}
