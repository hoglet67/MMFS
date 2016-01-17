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
die "Syntax: $BeebUtils::PROG [-f MMB_file] image_number\n" unless defined($dr);
die "Not a number\n" unless $dr=~/^[0-9]+$/;

my %disk=BeebUtils::load_dcat();
die "Disk $dr not valid\n" unless $disk{$dr}{Formatted};

my $L=$disk{$dr}{ReadOnly}?" (L)":"";
print "Catalogue for Disk $dr: $disk{$dr}{DiskTitle}$L\n";

my $image=BeebUtils::read_ssd($dr);

my %files=BeebUtils::read_cat(\$image);

BeebUtils::print_cat(%files);
