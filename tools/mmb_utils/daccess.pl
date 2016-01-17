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
die "Syntax: $BeebUtils::PROG [-f MMB_file] image_number [L]\n" unless defined($dr);
die "Not a number\n" unless $dr=~/^[0-9]+$/;

my $lock=0;
$lock=1 if $ARGV[1];

my $disktable=BeebUtils::LoadDiskTable();
my %disk=BeebUtils::load_dcat();

die "Disk $dr not formatted.\n" unless $disk{$dr}{Formatted};
BeebUtils::lock_disk($dr,$lock,\$disktable);
BeebUtils::SaveDiskTable(\$disktable);
