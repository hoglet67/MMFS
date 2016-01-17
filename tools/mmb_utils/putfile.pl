#!/usr/bin/perl -w
use strict;

use FindBin;
use lib "$FindBin::Bin";
use BeebUtils;

@ARGV=BeebUtils::init_ssd(@ARGV);

my $compact=0;
if (@ARGV && $ARGV[0] eq '-c') { $compact=1 ; shift @ARGV; }

die "$BeebUtils::PROG SSD [-c] file(s)\n" unless @ARGV && $BeebUtils::BBC_FILE;

my $image=BeebUtils::load_external_ssd(undef,1);

# This is an optimization.  The Beeb always adds a file to the end of
# the catalogue, even if there's a gap in the middle.  Simple code for
# simple times.  We can do the same thing 'cos it's simple.  But we can
# _compact_ the image first 'cos we're not throwing physical heads around
# and so we'll always have the most free space possible
BeebUtils::compact_ssd(\$image) if $compact;

foreach (@ARGV)
{
  next unless -f $_;
  # If this is an inf file and there's a file of the same name...
  # skip!
  if (/\.inf$/)
  {
    my $b=$_; $b=~s/.inf$//;
    next if -e $b;
  }
  BeebUtils::add_file_to_ssd(\$image,$_);
}

BeebUtils::write_ssd(\$image,$BeebUtils::BBC_FILE);
