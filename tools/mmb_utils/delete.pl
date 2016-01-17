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

my $force;
if (@ARGV && $ARGV[0] eq '-y')
{
  $force=1; shift @ARGV;
}

die "Syntax: $BeebUtils::PROG SSD [-y] file(s)\n" unless @ARGV && $BeebUtils::BBC_FILE;

my $image=BeebUtils::load_external_ssd(undef,1);

BeebUtils::delete_files($force,\$image,@ARGV);
