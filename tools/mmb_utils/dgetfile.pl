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
my $destdir=$ARGV[1];
die "Syntax: $BeebUtils::PROG [-f MMB_file] image_number destdir\n" unless defined($destdir);
die "Not a number\n" unless $dr=~/^[0-9]+$/;
die "$destdir already exists\n" if -e $destdir;

my %disk=BeebUtils::load_dcat();
die "Disk $dr not valid\n" unless $disk{$dr}{Formatted};

my $L=$disk{$dr}{ReadOnly}?" (L)":"";
print "Extracting from disk $dr: $disk{$dr}{DiskTitle}$L\n";

mkdir($destdir) || die "mkdir $destdir: $!\n";

my $image=BeebUtils::read_ssd($dr);

chdir($destdir) || die "chdir $destdir: $!\n";
BeebUtils::save_all_files_from_ssd(\$image,1);
