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
die "Syntax: $BeebUtils::PROG filename.ssd\n" if $BeebUtils::BBC_FILE eq "";

my $image=BeebUtils::load_external_ssd(undef,0);

my %files=BeebUtils::read_cat(\$image);

BeebUtils::print_cat(%files);
