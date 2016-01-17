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
my $src0=$BeebUtils::BBC_FILE;
my ($src2,$dest)=@ARGV;
die "Syntax: $BeebUtils::PROG side0.ssd side2.ssd merged_disk.dsd\n" unless $dest;

die "$dest already exists\n" if -e $dest;
 
my $SIZE=256*10; # 10 sectors per track

my $src0_image=BeebUtils::load_external_ssd($src0,0);
my $src2_image=BeebUtils::load_external_ssd($src2,0);

# Ensure the disks are big enough; crummy non-ssd SSD images!
$src0_image .= "\0" x ($SIZE*80*2);
$src2_image .= "\0" x ($SIZE*80*2);

my $dest_image="";

foreach my $track (0..79)
{
  my $offset=$track*$SIZE;
  $dest_image .= substr($src0_image,$offset,$SIZE);
  $dest_image .= substr($src2_image,$offset,$SIZE);
}

BeebUtils::write_ssd(\$dest_image,$dest);
print "Disks merged\n";
