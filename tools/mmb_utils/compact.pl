#!/usr/bin/perl -w
use strict;

# Beeb Utilities to manipulate MMB and SSD files
# Copyright (C) 2012 Stephen Harris
# 
# See file "COPYING" for GPLv2 licensing

use FindBin;
use lib "$FindBin::Bin";
use BeebUtils;

@ARGV=BeebUtils::init_ssd(@ARGV);
die "Syntax: $BeebUtils::PROG SSD\n" unless $BeebUtils::BBC_FILE;

my $image=BeebUtils::load_external_ssd(undef,1);
if (BeebUtils::compact_ssd(\$image))
{
  BeebUtils::write_ssd(\$image,$BeebUtils::BBC_FILE);
  print "Disk compacted\n";
}
else
{ 
  print "No action needed\n";
}
