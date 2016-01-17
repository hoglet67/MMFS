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
my $force=0;
if ($ARGV[0] eq '-y') { $force=1; shift @ARGV; }

my $dr=$ARGV[0];
die "Syntax: $BeebUtils::PROG [-f MMB_file] [-y] image_number [R]\n" unless defined($dr);
die "Not a number\n" unless $dr=~/^[0-9]+$/;

my $restore=0; $restore=1 if $ARGV[1];

my $disktable=BeebUtils::LoadDiskTable();
my %disk=BeebUtils::load_dcat();
die "Disk $dr not valid\n" if !$disk{$dr}{Formatted} && !$restore;
die "Disk $dr not deleted\n" if $disk{$dr}{Formatted} && $restore;

die "Disk $dr ($disk{$dr}{DiskTitle}) is locked\n" if $disk{$dr}{ReadOnly} && !$force;

print "Deleting disk $dr: $disk{$dr}{DiskTitle}\n";
if (!$restore && !$force)
{
  print "Are you sure (Y/N)? ";
  my $x=<STDIN>;
  if ($x!~/^[Yy]/) { exit; }
}

BeebUtils::DeleteSlot($dr,$restore,\$disktable);
BeebUtils::SaveDiskTable(\$disktable);

print $restore?"Restored\n":"Removed\n";
