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
die "Syntax: $BeebUtils::PROG filename.ssd [0-3]\n" if $BeebUtils::BBC_FILE eq "" || !@ARGV || $ARGV[0] !~ /^[0-3]$/;

my $image=BeebUtils::load_external_ssd(undef,1);
BeebUtils::opt4(\$image,$ARGV[0]);
BeebUtils::write_ssd(\$image,$BeebUtils::BBC_FILE);
