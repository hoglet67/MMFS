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
my $title=$ARGV[0];
die "Syntax: $BeebUtils::PROG filename.ssd title\n" if $dest eq "" || !$title;

my $image=BeebUtils::load_external_ssd(undef,1);
BeebUtils::set_ssd_title(\$image,$title);
BeebUtils::write_ssd(\$image,$dest);
print "$dest updated\n";
