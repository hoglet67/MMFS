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

my ($disktable,%boot)=BeebUtils::load_onboot();
my %disk=BeebUtils::load_dcat(\$disktable);

if (!@ARGV)
{
  foreach (0..3)
  {
    my $d=$boot{$_};
    my $t="<empty>";
    if ($disk{$d}{Formatted})
    {
      my $L=$disk{$d}{ReadOnly}?" (L)":"";
      $t="$disk{$d}{DiskTitle}$L";
    }
    print "$_: $d - $t\n";
  }
  exit;
}

my $force=0;
if ($ARGV[0] eq '-y') { $force=1; shift @ARGV; }
my $drive=shift @ARGV;
my $disk=shift @ARGV;
die "Syntax: $BeebUtils::PROG [-y] drive disk_number\n" unless defined($disk);
die "Invalid drive\n" unless $drive=~/^[0123]$/;
die "Invalid disk\n" unless $disk=~/^\d+$/;

die "Disk $disk not formatted.  Use -y flag to foce\n" unless $disk{$disk}{Formatted} || $force;
$boot{$drive}=$disk;
BeebUtils::save_onboot(\$disktable,%boot);
