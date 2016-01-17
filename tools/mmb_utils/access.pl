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

die "Syntax: $BeebUtils::PROG SSD filename [L]\n" unless @ARGV && $BeebUtils::BBC_FILE;

my $filename=$ARGV[0];
my $lock=$ARGV[1]?1:0;

my $image=BeebUtils::load_external_ssd(undef,1);

BeebUtils::lock_files($filename,$lock,\$image);
