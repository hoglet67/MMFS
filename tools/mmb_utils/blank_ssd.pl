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
my $dest=$BeebUtils::BBC_FILE;
die "Syntax: $BeebUtils::PROG filename.ssd\n" unless $dest;

die "$dest already exists\n" if -e $dest;
 
my $image=BeebUtils::blank_ssd();
BeebUtils::write_ssd(\$image,$dest);
print "Blank $dest created\n";
