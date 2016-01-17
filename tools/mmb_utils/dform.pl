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

my $dr=$ARGV[0];
die "Syntax: $BeebUtils::PROG [-f MMB_file] image_number [title]\n" unless defined($dr);
die "Not a number\n" unless $dr=~/^[0-9]+$/;

my $title=$ARGV[1] || '';

my $disktable=BeebUtils::LoadDiskTable;
my %disk=BeebUtils::load_dcat(\$disktable);
die "Disk $dr already in use; use dkill to erase to reuse.\n" if $disk{$dr}{Formatted};

my $image=BeebUtils::blank_ssd();
BeebUtils::set_ssd_title(\$image,$title);
BeebUtils::put_ssd($image,$dr);

BeebUtils::DeleteSlot($dr,1,\$disktable);  # set's disk type to RW
BeebUtils::ChangeDiskName($dr,$title,\$disktable);
BeebUtils::SaveDiskTable(\$disktable);

print "Blank disk ($title) written to $dr\n";
