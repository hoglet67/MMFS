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
die "Syntax: $BeebUtils::PROG [-f MMB_file] image_number new_label\n" unless defined($dr);
die "Not a number\n" unless $dr=~/^[0-9]+$/;

my $target=$ARGV[1];
die "No label\n" unless $target;

my $disktable=BeebUtils::LoadDiskTable();
my %disk=BeebUtils::load_dcat();
die "Disk $dr not valid\n" unless $disk{$dr}{Formatted};
die "Disk $dr ($disk{$dr}{DiskTitle}) is locked\n" if $disk{$dr}{ReadOnly};

print "Getting disk $dr: $disk{$dr}{DiskTitle}\n";

BeebUtils::ChangeDiskName($dr,$target,\$disktable);
BeebUtils::SaveDiskTable(\$disktable);

# Reload the MMB catalogue
%disk=BeebUtils::load_dcat();
print "Set to disk $dr: $disk{$dr}{DiskTitle}\n";
