package BeebUtils;

# Beeb Utilities to manipulate MMB and SSD files
# Copyright (C) 2012 Stephen Harris
# 
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

use warnings;
use strict;
use FileHandle;

# Constants
my $SecSize=256;
my $DiskTableSize=32*$SecSize;
my $MaxDisks = ($DiskTableSize/16)-1;  # slot 0 isn't a real disk

my $DiskCatalogueSize = 2 * $SecSize ; # DFS
my $DiskSectors = 800 ; # Only size supported! 80track single density
my $DiskSize = $DiskSectors * $SecSize;

my $CatalogueMaxFiles = 31;  # Acorn maximum.  Solidisk can exceed this
                             # but we only read that format

# Disk table values
my $DiskReadOnly = 0;
my $DiskReadWrite = 0xF;
my $DiskUnformatted = 0xF0;
my $DiskInvalid = 0xFF;

# Almost a constant.  What we're gonna do here is nasty, but it means
# that any program that uses this module will automatically gain an
# optional "-f filename"
my $DEFAULT_BBC_FILE=defined $ENV{'BBC_FILE'} ? $ENV{'BBC_FILE'} : "BEEB.MMB";

our $BBC_FILE="";

# These values extracted from BASIC 2 ROM
# starting at &8071 and ending at &836C
# (see extract_tokens for how this was built)

# This is "our" so external programs can see it
our %basic_tokens;
our %extended_tokens;

$basic_tokens{"_BASE_"}=
{
  128 => ['AND',0x00],     192 => ['LEFT$(',0x00],
  129 => ['DIV',0x00],     193 => ['MID$(',0x00],
  130 => ['EOR',0x00],     194 => ['RIGHT$(',0x00],
  131 => ['MOD',0x00],     195 => ['STR$',0x00],
  132 => ['OR',0x00],      196 => ['STRING$(',0x00],
  133 => ['ERROR',0x04],   197 => ['EOF',0x01],
  134 => ['LINE',0x00],    198 => ['AUTO',0x10],
  135 => ['OFF',0x00],     199 => ['DELETE',0x10],
  136 => ['STEP',0x00],    200 => ['LOAD',0x02],
  137 => ['SPC',0x00],     201 => ['LIST',0x10],
  138 => ['TAB(',0x00],    202 => ['NEW',0x01],
  139 => ['ELSE',0x14],    203 => ['OLD',0x01],
  140 => ['THEN',0x14],    204 => ['RENUMBER',0x10],
  142 => ['OPENIN',0x00],  205 => ['SAVE',0x02],
  143 => ['PTR',0x43],     207 => ['PTR',0x00],
  144 => ['PAGE',0x43],    208 => ['PAGE',0x00],
  145 => ['TIME',0x43],    209 => ['TIME',0x00],
  146 => ['LOMEM',0x43],   210 => ['LOMEM',0x00],
  147 => ['HIMEM',0x43],   211 => ['HIMEM',0x00],
  148 => ['ABS',0x00],     212 => ['SOUND',0x02],
  149 => ['ACS',0x00],     213 => ['BPUT',0x03],
  150 => ['ADVAL',0x00],   214 => ['CALL',0x02],
  151 => ['ASC',0x00],     215 => ['CHAIN',0x02],
  152 => ['ASN',0x00],     216 => ['CLEAR',0x01],
  153 => ['ATN',0x00],     217 => ['CLOSE',0x03],
  154 => ['BGET',0x01],    218 => ['CLG',0x01],
  155 => ['COS',0x00],     219 => ['CLS',0x01],
  156 => ['COUNT',0x01],   220 => ['DATA',0x20],
  157 => ['DEG',0x00],     221 => ['DEF',0x00],
  158 => ['ERL',0x01],     222 => ['DIM',0x02],
  159 => ['ERR',0x01],     223 => ['DRAW',0x02],
  160 => ['EVAL',0x00],    224 => ['END',0x01],
  161 => ['EXP',0x00],     225 => ['ENDPROC',0x01],
  162 => ['EXT',0x01],     226 => ['ENVELOPE',0x02],
  163 => ['FALSE',0x01],   227 => ['FOR',0x02],
  164 => ['FN',0x08],      228 => ['GOSUB',0x12],
  165 => ['GET',0x00],     229 => ['GOTO',0x12],
  166 => ['INKEY',0x00],   230 => ['GCOL',0x02],
  167 => ['INSTR(',0x00],  231 => ['IF',0x02],
  168 => ['INT',0x00],     232 => ['INPUT',0x02],
  169 => ['LEN',0x00],     233 => ['LET',0x04],
  170 => ['LN',0x00],      234 => ['LOCAL',0x02],
  171 => ['LOG',0x00],     235 => ['MODE',0x02],
  172 => ['NOT',0x00],     236 => ['MOVE',0x02],
  173 => ['OPENUP',0x00],  237 => ['NEXT',0x02],
  174 => ['OPENOUT',0x00], 238 => ['ON',0x02],
  175 => ['PI',0x01],      239 => ['VDU',0x02],
  176 => ['POINT(',0x00],  240 => ['PLOT',0x02],
  177 => ['POS',0x01],     241 => ['PRINT',0x02],
  178 => ['RAD',0x00],     242 => ['PROC',0x0A],
  179 => ['RND',0x01],     243 => ['READ',0x02],
  180 => ['SGN',0x00],     244 => ['REM',0x20],
  181 => ['SIN',0x00],     245 => ['REPEAT',0x00],
  182 => ['SQR',0x00],     246 => ['REPORT',0x01],
  183 => ['TAN',0x00],     247 => ['RESTORE',0x12],
  184 => ['TO',0x00],      248 => ['RETURN',0x01],
  185 => ['TRUE',0x01],    249 => ['RUN',0x01],
  186 => ['USR',0x00],     250 => ['STOP',0x01],
  187 => ['VAL',0x00],     251 => ['COLOUR',0x02],
  188 => ['VPOS',0x01],    252 => ['TRACE',0x12],
  189 => ['CHR$',0x00],    253 => ['UNTIL',0x02],
  190 => ['GET$',0x00],    254 => ['WIDTH',0x02],
  191 => ['INKEY$',0x00],  255 => ['OSCLI',0x02]
};

$basic_tokens{"basic2"}={};
$extended_tokens{"basic2"}={};

# These extensions from http://mdfs.net/Docs/Comp/BBCBasic/Tokens
$basic_tokens{"basic4"}= { 206 => ['EDIT',0] };
$extended_tokens{"basic4"}={};

$basic_tokens{"z80"}= { 206 => ['PUT',0] };
$extended_tokens{"z80"}={};

$basic_tokens{"arm"}=
{
  127 => ['OTHERWISE',0],   204 => ['ELSE',0],
  201 => ['WHEN',0],        205 => ['ENDIF',0],
  202 => ['OF',0],          206 => ['ENDWHILE',0],
  203 => ['ENDCASE',0]
};
$extended_tokens{"arm"}{198}={ 142 => ['SUM',0], 143 => ['BEAT',0] };
$extended_tokens{"arm"}{199}=
{
  142 => ['APPEND',0],   151 => ['NEW',0],
  143 => ['AUTO',0],     152 => ['OLD',0],
  144 => ['CRUNCH',0],   153 => ['RENUMBER',0],
  145 => ['DELETE',0],   154 => ['SAVE',0],
  146 => ['EDIT',0],     155 => ['TEXTLOAD',0],
  147 => ['HELP',0],     156 => ['TEXTSAVE',0],
  148 => ['LIST',0],     157 => ['TWIN',0],
  149 => ['LOAD',0],     158 => ['TWINO',0],
  150 => ['LVAR',0],     159 => ['INSTALL',0]
};
$extended_tokens{"arm"}{200}=
{
  142 => ['CASE',0],       155 => ['LIBRARY',0],
  143 => ['CIRCLE',0],     156 => ['TINT',0],
  144 => ['FILL',0],       157 => ['ELLIPSE',0],
  145 => ['ORIGIN',0],     158 => ['BEATS',0],
  146 => ['POINT',0],      159 => ['TEMPO',0],
  147 => ['RECTANGLE',0],  160 => ['VOICES',0],
  148 => ['SWAP',0],       161 => ['VOICE',0],
  149 => ['WHILE',0],      162 => ['STEREO',0],
  150 => ['WAIT',0],       163 => ['OVERLAY',0],
  151 => ['MOUSE',0],      164 => ['MANDEL',0],
  152 => ['QUIT',0],       165 => ['PRIVATE',0],
  153 => ['SYS',0],        166 => ['EXIT',0],
  154 => ['INSTALL',0]
};

$basic_tokens{"b4w"}=
{
    1 => ['CIRCLE',0],     198 => ['SUM',0],
    2 => ['ELLIPSE',0],    199 => ['WHILE',0],
    3 => ['FILL',0],       200 => ['CASE',0],
    4 => ['MOUSE',0],      201 => ['WHEN',0],
    5 => ['ORIGIN',0],     202 => ['OF',0],
    6 => ['QUIT',0],       203 => ['ENDCASE',0],
    7 => ['RECTANGLE',0],  204 => ['ELSE',0],
    8 => ['SWAP',0],       205 => ['ENDIF',0],
    9 => ['SYS',0],        206 => ['ENDWHILE',0],
   10 => ['TINT',0],
   11 => ['WAIT',0],
   12 => ['INSTALL',0],
   14 => ['PRIVATE',0],
   15 => ['BY',0],
   16 => ['EXIT',0],
};
$extended_tokens{"b4w"}={};

my $SOLIDISK=0;
my $WATFORD=0;
my $OPUS=0;
my $DISK_DOCTOR=0;

sub init(@)
{
  my (@arg)=@_;
  if (@arg >=2 && $arg[0] eq '-f')
  {
    $BBC_FILE=$arg[1];
    shift @arg;
    shift @arg;
  }
  elsif (@arg && $arg[0] =~ /^-f(.+)$/)
  {
    $BBC_FILE=$1;
    shift @arg;
  }
  elsif (@arg==1 && $arg[0] eq '-f')
  {
    die "Missing filename argument to -f\n";
  }

  return(@arg);
}

sub init_ssd(@)
{
  my (@arg)=@_;
  if (@arg)
  {
    my $f=$arg[0];
    if ($f=~/^([^:]+):(.+)$/)
    {
      $f=$2;
      check_and_set_type($1);
    }
    $BBC_FILE=$f;
    shift @arg;
  }
  return (@arg);
}

my $file_handle=undef;

# Open BBC_FILE if it's not already open
sub OpenFile()
{
  return if $file_handle;
  $BBC_FILE=$DEFAULT_BBC_FILE unless $BBC_FILE;
  die "$BBC_FILE is not a file!\n" unless -f $BBC_FILE;
  $file_handle = new FileHandle("+< $BBC_FILE");
  die "Could not open $BBC_FILE: $!\n" unless $file_handle;
  binmode($file_handle);
  return;
}

sub LoadDiskTable()
{
  my $disktable;
  OpenFile;
  sysseek($file_handle,0,0);
  sysread($file_handle,$disktable,$DiskTableSize);
  return $disktable;
}

sub SaveDiskTable($)
{
  my ($disktable)=@_;
  OpenFile;
  sysseek($file_handle,0,0);
  syswrite($file_handle,$$disktable,$DiskTableSize);
}

# Disk slot and new title and table
sub ChangeDiskName($$$)
{
  my ($slot,$title,$disktable)=@_;
  return if $slot < 0 || $slot >= $MaxDisks;
  $title.=("\0"x15); $title=substr($title,0,15);
  substr($$disktable,$slot*16+16,15)=$title;
}

# Delete slot
sub DeleteSlot($$$)
{
  my ($slot,$restore,$disktable)=@_;
  return if $slot < 0 || $slot >= $MaxDisks;
  substr($$disktable,$slot*16+16+15,1)=chr($restore?$DiskReadWrite:$DiskUnformatted);
}

# Lock/Unlock a slot
sub lock_disk($$$)
{
  my ($slot,$lock,$disktable)=@_;
  return if $slot < 0 || $slot >= $MaxDisks;
  substr($$disktable,$slot*16+16+15,1)=chr($lock?$DiskReadOnly:$DiskReadWrite);
}

# disk is disk slot on the MMB (0->$MaxDisks-1)
# disktable is a reference to a already loaded disktable
#  eg
#    my $disktable=LoadDiskTable;
#    my ($title,$type)=GetDskName(10,\$disktable);
sub GetDskName($$)
{
  my ($disk,$disktable)=@_;
  my $offset=$disk*16+16;
 
  my $title=substr($$disktable,$offset,12); $title =~ s/\0.*$//;
  my $type=substr($$disktable,$offset+15,1); $type=ord($type);
  return ($title,$type);
}

# Returns a simple hashref of boot image and the disk catalog
sub load_onboot()
{
  my $disktable=LoadDiskTable;
  my %boot;
  foreach (0..3)
  {
    $boot{$_}=ord(substr($disktable,$_,1))+ord(substr($disktable,$_+4,1))*256;
  }
  return ($disktable,%boot);
}

sub save_onboot($%)
{
  my ($disktable,%boot)=@_;
  foreach (0..3)
  {
    substr($$disktable,$_,1)=chr($boot{$_} & 0xff);
    substr($$disktable,$_+4,1)=chr(($boot{$_} & 0xff00) >> 8);
  }
  SaveDiskTable($disktable);
}

sub DiskPtr($;$)
{
  my ($DiskNo,$Sec)=@_;
  $Sec=0 unless $Sec;
  return $DiskTableSize+($DiskNo*$DiskSize)+($Sec*$SecSize);
}

sub BootOpt($)
{
  my ($bytOption)=@_;

  if ($bytOption == 0) { return 'None'; }
  if ($bytOption == 1) { return 'LOAD'; }
  if ($bytOption == 2) { return 'RUN'; }
  if ($bytOption == 3) { return 'EXEC'; }
  return "";
}

sub DiskSize($)
{
  my ($size)=@_;

  # If we want more detail, comment this line out...
  return int($size/4) . "K";

  if ($size == 0x190) { return "100K - 40x10 - SD"; }
  if ($size == 0x280) { return "160K - 40x16 - DD"; }
  if ($size == 0x2d0) { return "180K - 40x18 - DD"; }
  if ($size == 0x320) { return "200K - 80x10 - SD"; }
  if ($size == 0x500) { return "320K - 80x16 - DD"; }
  if ($size == 0x5a0) { return "360K - 80x18 - DD"; }
  return int($size/4) . "K";
}

# Loads the main catalog from the MMB file
# (optionally pass an already loaded disktable)
sub load_dcat(;$)
{
  my ($tbl)=@_;

  my ($disktable,%disk);

  if ($tbl)
  {
    $disktable=$$tbl;
  }
  else
  {
    $disktable=LoadDiskTable;
  }

  foreach (0..$MaxDisks-1)
  {
    $disk{$_}{ValidDisk}=0;
    $disk{$_}{Formatted}=0;
    $disk{$_}{ReadOnly}=0;
    $disk{$_}{DiskTitle}="";

    my ($title,$type)=GetDskName($_,\$disktable);

    if ($type == $DiskReadOnly || $type == $DiskReadWrite)
    {
      $disk{$_}{ValidDisk}=1;
      $disk{$_}{Formatted}=1;
      $disk{$_}{ReadOnly}=($type == $DiskReadOnly)?1:0;
      $disk{$_}{DiskTitle}=$title;
    }
    elsif ($type == $DiskUnformatted)
    {
      $disk{$_}{ValidDisk}=1;
      $disk{$_}{DiskTitle}=$title;
    }
  }
  return %disk;
}

sub blank_mmb()
{
  my $image="\0" x (DiskPtr($MaxDisks));  # Maybe large!
  substr($image,0,4)="\0\1\2\3"; # Default onboot disks
  foreach (1..$MaxDisks)
  {
    substr($image,$_*16+15,1)=chr($DiskUnformatted);
  }

  return($image);
}

sub blank_ssd()
{
  my $image="\xE5" x $DiskSize;
  substr($image,0,512)="\x0" x 512;
  substr($image,0x104,4)="\x01\x00\x03\x20";  # 200K disk
  return($image);
}

# Reads an SSD image from an MMB
sub read_ssd(;$)
{
  my ($disk)=@_;
  OpenFile;
  sysseek($file_handle,DiskPtr($disk),0) if defined($disk);
  my $image;
  sysread($file_handle,$image,$DiskSize);
  # Ensure the image is at least the right size
  die "Image is too small; not even 2 sectors!\n" if length($image)<512;
  $image .= blank_ssd();
  $image=substr($image,0,$DiskSize);
  return($image);
}

# Reads an SSD from an external file
sub load_external_ssd(;$$)
{
  my ($fname,$size_check)=@_;
  my $target=$fname||$BBC_FILE;
  
  die "$target is not a file!\n" unless -f $target;
  my $f=new FileHandle "<$target";
  die "Could not open $target: $!\n" unless $f;
  binmode($f);
  my $image;
  # We won't read more than 400K, regardless
  sysread($f,$image,409600);
  if ($size_check && length($image)> $DiskSize)
  {
    die "File $target is over $DiskSize in size\n";
  }
  close($f);

  # Ensure the image is at least the right size
  die "Image is too small; not even 2 sectors!\n" if length($image)<512;
  if (length($image) < $DiskSize)
  {
    $image .= blank_ssd();
    $image=substr($image,0,$DiskSize);
  }
  return($image);
}

sub put_ssd($$)
{
  my ($image,$disk)=@_;
  OpenFile;
  sysseek($file_handle,DiskPtr($disk),0);
  
  syswrite($file_handle,$image,$DiskSize);
}

sub write_ssd($$)
{
  my ($image,$filename)=@_;

  my $fh=new FileHandle ">$filename";
  die "Can not open $filename for saving\n" unless $fh;
  binmode($fh);
  print $fh $$image;
  close($fh);
}

sub set_ssd_title($$)
{
  my ($image,$title)=@_;

  $title .= ("\0"x12);
  my $t1=substr($title,0,8);
  my $t2=substr($title,8,4);

  # If Solidisk secondary catalog then cut down title by 2
  my @b=unpack("C",substr($$image,0x102,1));
  $t2=substr($t2,0,2) if ($b[0] & 192) == 192;
  substr($$image,0,8)=$t1;
  substr($$image,0x100,length($t2))=$t2;
}

# Reads a standard BBC DFS catalogue.
#  $image is a ref to a loaded disk image
#  $start is a sector offset into the image to find the catalog
sub _read_ssd_cat($$)
{
  my ($image,$start)=@_;
  my $offset=$start*256;
  
  my %files;

  # BBC disk format: First 8 bytes are part of the disk title; null terminated
  my $disk_title=substr($$image,$offset,8);

  # We'll grab the filecount from sector 1 'cos that makes it easier...
  my $filecount=ord(substr($$image,$offset+256+5,1))/8;

  # Next lot of 8 data are filenames
  foreach (0..$filecount-1)
  {
    my $t=substr($$image,$offset+$_*8+8,7); $t=~s/ .*$//;
    my $d=substr($$image,$offset+$_*8+8+7,1); $d=ord($d);

    my $locked=($d>127)?1:0;
    $d=chr($d%128);
    $t="$d.$t"; $t=~s/\0.*$//;
    $files{$_}{locked}=$locked;
    $files{$_}{name}="$t";
    $files{$_}{cat_sector}=$start;
  }

  # Second sector.
  # First 4 bytes are also part of the title
  my $t2=substr($$image,$offset+256,4); $disk_title .= $t2;

  # Disk title is null terminated.  But quick check to see if's a solidisk
  # chained catalogue.  No one should use high-bit characters in disk
  # titles, so this is relative safe to do.
  my $chained;
  if ($SOLIDISK)
  {
    my @b=unpack("C2",substr($disk_title,10,2));
    if (($b[0] & 192) == 192)
    {
      $chained=($b[0]&63)*256+$b[1];
      $disk_title=substr($disk_title,0,10);
    }
  }
  $disk_title=~s/\0.*$//;

  # next byte is BCD cycle
  my $cycle=substr($$image,$offset+256+4,1); $cycle=ord($cycle);
  $cycle=int($cycle/16)*10+($cycle%16);

  # (We'd already got the filecount, earlier)

  # Now the last next two bytes encode both *OPT4 value and disk size
  my @b=unpack("C2",substr($$image,$offset+256+6,2));

  my $opt4=int($b[0]/16) & 3;
  # Really this should only be &3 but DD disks need 11 bits, so...
  # (relatively safe to do everywhere - bit 3 was unused, otherwise)
  my $disk_size=($b[0]&7)*256+$b[1];

  # Now we have entries for load/exec/size/start-sec
  # LL LL EE EE SS SS XX YY
  # Load  Exec  Size     Sector
  # The first 6 bytes are load/exec/size bottom 16 bits.
  # The 8th byte encodes the low 8 bits for start-sec
  # The 7th byte is special.  For Acorn DFS:
  # bits 0+1 are high bits of sector start
  # bits 2+3 are high bits of load address
  # bits 4+5 are high bits of size
  # bits 6+7 are high bits of exec address
  # 
  # That makes 10 bits for start sector and 18 bits for size.
  # 
  # But on a 320K disk you need 11 bits and 19 bits.  So Solidisk steals
  # bits 2 and 3 ("load address").  bit 2 is added to the sector, bit 3 to
  # the size.  This means we have now only have 8 bits for the load address.
  # Solidisk re-uses the exec high bits for the load high bits.
  
  foreach (0..$filecount-1)
  {
    my ($load,$exec,$size,$sec);
    @b=unpack("C2",substr($$image,$offset+256+$_*8+8  ,2)); $load=$b[1]*256+$b[0];
    @b=unpack("C2",substr($$image,$offset+256+$_*8+8+2,2)); $exec=$b[1]*256+$b[0];
    @b=unpack("C2",substr($$image,$offset+256+$_*8+8+4,2)); $size=$b[1]*256+$b[0];

    # Higher bits are encoded
    @b=unpack("C",substr($$image,$offset+256+$_*8+8+6,1));
    my $highsec  = ($b[0]&3);            # bits 0 and 1
    my $highload = ($b[0] & 0xc)  >> 2; # bits 2 and 3
    my $highsize = ($b[0] & 0x30) >> 4; # bits 4 and 5
    my $highexec = ($b[0] & 0xc0) >> 6; # bits 6 and 7

    if ($SOLIDISK && $disk_size == 0x500)
    {
      if ($highload & 1) { $highsec  += 4; }
      if ($highload & 2) { $highsize += 4; }
      $highload=$highexec;
    }

    # Watford does it slightly differently and uses high bits in the
    # filename which means we'll need to recalculate it.
    if ($WATFORD)
    {
      my $name=$files{$_}{name};
      @b=unpack("C2",substr($$image,$offset+$_*8+8+5,2));
      if ($b[0]>127)
      {
        substr($name,5,1)=chr($b[0] & 127) if length($name)>5;
        $highsize += 4;
      }
      if ($b[1]>127)
      {
        substr($name,6,1)=chr($b[1] & 127) if length($name)>6;
        $highsec += 4;
      }
      $files{$_}{name}=$name;
    }  
    $highload=0xff if $highload == 3;
    $highexec=0xff if $highexec == 3;

    @b=unpack("C",substr($$image,$offset+256+$_*8+8+7,1));
    $sec=$b[0];

    $files{$_}{load}=$load+$highload*65536;
    $files{$_}{exec}=$exec+$highexec*65536;
    $files{$_}{size}=$size+$highsize*65536;
    $files{$_}{start}=$sec+$highsec*256;
  }

  # Do we have multi-catalogues?  We've already checked for solidisk, earlier
  # what about the others?
  if ($WATFORD && $start==0)
  {
    if (substr($$image,512,8) eq "\xAA"x8)
    {
      $chained=2;  # Watford DFS allows another catalogue at sector 2
    }
  }

  if ($start == 0)
  {
    $files{""}{title}=$disk_title;
    $files{""}{cycle}=$cycle;
    $files{""}{disk_size}=$disk_size;
    $files{""}{opt4}=$opt4;
  }
  $files{""}{filecount}=$filecount;
  $files{""}{chain}=$chained;

  return %files;
}

# Read disk catalgue.  Follow chain if multi-catalogues
# If $0 is a reference to a BBC disk image
sub read_cat($)
{
  my ($image)=@_;

  # OPUS is sufficiently different...
  if ($OPUS) { return read_cat_opus($image); }

  my %files;

  my $max_file=0;
  my $chain=0;
  my $deleted=0;

  while ($chain ne "")
  {
    my %this_cat=_read_ssd_cat($image,$chain);
    $files{""}{last_cat}=$chain;
    if ($chain == 0)
    {
      foreach my $k (keys %{$this_cat{""}})
      {
        $files{""}{$k}=$this_cat{""}{$k};
      }
    }
    else
    {
      $files{""}{filecount}+=$this_cat{""}{filecount};
    }

    my $nextchain=$this_cat{""}{chain} || "";
    delete($this_cat{""});

    foreach (sort { $a <=> $b } keys %this_cat)
    {
      next if $_ eq "";
      # Skip deleted files from multi-catalog (directory is 0x7F
      # and file is locked
      if ($SOLIDISK && $this_cat{$_}{name} =~ /^\x7f\./ && $this_cat{$_}{locked})
      {
        $deleted++;
        next;
      }
      # Is this a hidden file that "blocks out" data from first catalogue?
      # If so, it's the end of this 2nd catalogue
      if ($SOLIDISK && $this_cat{$_}{name} eq '?.???????' && $this_cat{$_}{locked} && $chain)
      {
        $deleted++;
        next;
      }

      #Disk doctor?
      if ($DISK_DOCTOR && $this_cat{$_}{name} eq '!.!!!!!!!' && $this_cat{$_}{locked})
      {
        $deleted++;
        $nextchain=2 unless $chain;
        next;
      }

      foreach my $k (keys %{$this_cat{$_}})
      {
        $files{$max_file}{$k}=$this_cat{$_}{$k};
      }
      $max_file++ unless $_ eq "";
    }

    $chain=$nextchain;
  }
  $files{""}{deleted}=$deleted;
  return %files;
}

# OPUS...
sub read_cat_opus($)
{
  my ($image)=@_;

  # if this isn't an opus image then abort 'cos it could
  # be almost anything
  # The four bytes should be 20 05 a0 12
  if (substr($$image,0x1000,4) ne "\x20\x05\xa0\x12")
  {
    die "This does not appear to be an OPUS DDOS image\n";
  }

  my %catstart;
  foreach my $cat (0..7)
  {
    my @b=unpack("C2",substr($$image,0x1008+$cat*2,2));
    # This is measured in tracks, so *18 for sector
    $catstart{$cat}=($b[0]+256*$b[1])*18;
#    printf "debug: %d at %03X\n",$cat,$catstart{$cat};
  }

  my %files;

  my $max_file=0;
  $files{""}{last_cat}=7; # DDOS always has multi-cat even if they're empty
  foreach my $chain (0..7)
  {
    next unless $catstart{$chain};

    my %this_cat=_read_ssd_cat($image,$chain*2);
    if ($chain == 0)
    {
      foreach my $k (keys %{$this_cat{""}})
      {
        $files{""}{$k}=$this_cat{""}{$k};
      }
      $files{""}{disk_size}=0x5a0;
    }
    else
    {
      $files{""}{filecount}+=$this_cat{""}{filecount};
    }
    delete($this_cat{""});

    foreach (sort { $a <=> $b } keys %this_cat)
    {
      next if $_ eq "";

      $this_cat{$_}{start}+=$catstart{$chain};
      foreach my $k (keys %{$this_cat{$_}})
      {
        $files{$max_file}{$k}=$this_cat{$_}{$k};
      }
      $max_file++ unless $_ eq "";
    }

  }
  $files{""}{deleted}=0;
  return %files;
}

# Reference to image; generate a CRC
sub CalcCrc($)
{
  my ($image)=@_;
  my $crc=0;

  foreach (0..length($$image)-1)
  {
    $crc ^= (256*ord(substr($$image,$_,1)));
    foreach my $x (0..7)
    {
      $crc *= 2;
      if ($crc > 65535)
      {
        $crc-=65535; $crc ^= 0x1020;
      }
    }
  }
  return($crc);
}
# Get a file from an SSD image
#  $0 is ref to an image
#  $1 is the filename
#  $2 is a catalogue; we'll build one if not provided
# If you provide a catalogue for a different image then you get
# all you deserve

sub ExtractFile($$;%)
{
  my ($image,$filename,%cat)=@_;

  my %files;
  if (%cat)
  {
    %files=%cat;
  }
  else
  {
    %files=read_cat($image);
  }

  my $file=undef;
  foreach (sort keys %files)
  {
    my $f=$files{$_}{name};
    next unless $f;
    $file=$_ if lc($filename) eq lc($f);
  }

  die "Can not find $filename in image\n" unless defined($file);

  my $start=$files{$file}{start}*256;
  my $size=$files{$file}{size};
  return substr($$image,$start,$size);
}

# SSD image saved to current directory
sub save_all_files_from_ssd($;$)
{
  my ($image,$verbose)=@_;

  my %files=read_cat($image);


  foreach (keys %files)
  {
    my $n=$files{$_}{name};
    next unless $n;

    my $file=ExtractFile($image,$n,%files);
    my $crc=CalcCrc(\$file);

  # These keep Unix filenames at least a little sane
    $n=~tr/ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_+.!@$%,-/_/c;
    $n=~s/^\$\.// unless $n eq '$.';

    if ( -e $n )
    {
      my $n1=$n;
      my $cnt=0;
      while ( -e $n1 ) { $n1 = $n . "-" . ($cnt++); }
      $n=$n1;
    }

    print "Saving $files{$_}{name} as $n\n" if $verbose;

    my $fh=new FileHandle ">$n";
    die "Can not open $n for saving\n" unless $fh;
    binmode($fh);
    print $fh $file;
    close($fh);

    $fh=new FileHandle ">$n.inf";
    die "Can not open $n.inf for saving\n" unless $fh;
    printf $fh "%-9s %6X %6X %sCRC=%04X",
                             $files{$_}{name},
                             $files{$_}{load},
                             $files{$_}{exec},
                             $files{$_}{locked}?"Locked ":"",
                             $crc;
    close($fh);
  }
}

sub add_content_to_ssd($$$$$$;$)
{
  my ($image,$fname,$data,$load,$exec,$locked,$filename)=@_;
  $filename=$fname unless defined($filename);

  die "Bad filename for $filename ($fname)\n" unless $fname=~/^.\./ && length($fname)<=9;

  my %files=read_cat($image);

  die "Can not operate on large/multi-catalogue disks\n" if $files{""}{chain};
  die "Can only operate on 200Kb disks\n" if $files{""}{disk_size} != 0x320;
  my $cnt=$files{""}{filecount};
  die "Catalogue full.  Can not add $filename\n" if $cnt == 31;

  # Calculate first free sector
  my $first_sect=$files{0}{start};
  my $len=$files{0}{size};
  # If we have no files...
  if ($cnt == 0)
  {
    $first_sect=2;
    $len=0;
  }

  # Convert to sectors;
  $len=int($len/256)+(($len%256)?1:0);
  $first_sect += $len;
  my $last_sect=$files{""}{disk_size};
  delete $files{""};

  my $freesect=$last_sect-$first_sect;

  my $fsize=length($data);
  if ($fsize > $freesect*256)
  {
    die "Not enough space to add $filename\n";
  }


  # Is this file already on the disk?
  foreach (keys %files)
  {
    die "File $fname already present: $filename\n" if defined($files{$_}{name}) && lc($files{$_}{name}) eq lc($fname);
  }

  # If we've got here then it's safe to add the file!
  $fname .= "        ";
  substr($fname,0,1)=chr(ord(substr($fname,0,1))|128) if $locked;
  substr($$image,16,240)=substr($$image,8,240);
  substr($$image,8,7)=substr($fname,2,7);
  substr($$image,15,1)=substr($fname,0,1);
  
  substr($$image,256+16,240)=substr($$image,256+8,240);
  substr($$image,0x105,1)=chr($cnt*8+8);
  substr($$image,0x108,2)=chr($load&255) . chr(($load>>8) & 255);
  substr($$image,0x10a,2)=chr($exec&255) . chr(($exec>>8) & 255);
  substr($$image,0x10c,2)=chr($fsize&255) . chr(($fsize>>8) & 255);

  # Next byte is a little complicated
  my $b=($exec & 0x30000) >>10;
  $b |= ($fsize & 0x30000) >>12;
  $b |= ($load & 0x30000) >>14;
  $b |= ($first_sect>>8);
  substr($$image,0x10e,1)=chr($b);

  substr($$image,0x10f,1)=chr($first_sect & 255);

  substr($$image,$first_sect*256,length($data))=$data;
}

sub add_file_to_ssd($$)
{
  my ($image,$filename)=@_;

  die "$filename is not a file!\n" unless -f $filename;
  my $fh=new FileHandle "< $filename";
  die "Can not open $filename: $!\n" unless $fh;
  binmode($fh);
  my $data;
  sysread($fh,$data,$DiskSize+3);  # This will always be too large.
  close($fh);
  
  my $fname=$filename;
  $fname=~s!^.*/!!;
  $fname="\$.$fname" unless $fname=~/^.\./;
  $fname=substr($fname,0,9);

  my $load=0;
  my $exec=0;
  my $locked=0;

  # If there's an INF file, get that information
  if ( -f "$filename.inf" )
  {
    $fh=new FileHandle "< $filename.inf";
    die "Can not open $filename.inf: $!\n" unless $fh;
    my $line=<$fh>;  chomp($line);
    close($fh);
    ($fname,$load,$exec,$locked)=split(/\s+/,$line);
    die "Bad load address $load for $filename\n" unless $load=~/^[0-9A-Fa-f]+$/;
    die "Bad exec address $exec for $filename\n" unless $exec=~/^[0-9A-Fa-f]+$/;
    $load=hex("0x$load");
    $exec=hex("0x$exec");
    $locked="" unless $locked;
    $locked=($locked =~ /Locked/i)?1:0;
  }

  add_content_to_ssd($image,$fname,$data,$load,$exec,$locked,$filename);
}

sub print_cat(%)
{
  my (%files)=@_;
  
  printf "Disk title: %s (%d)  Disk size: &%03X - %s\n",
                      $files{""}{title},
                      $files{""}{cycle},
                      $files{""}{disk_size},
                      BeebUtils::DiskSize($files{""}{disk_size});

  my $count=$files{""}{filecount}-$files{""}{deleted};
  printf "Boot Option: %s (%s)   File count: %d\n", $files{""}{opt4},
           BeebUtils::BootOpt($files{""}{opt4}), $count;

  my $chain=$files{""}{chain};
  printf("(Multi-catalogues; last at &%02X)\n",$files{""}{last_cat}) if $chain;

  delete($files{""});

  print "\n";
  print "Filename:  Lck Lo.add Ex.add Length Sct";
  print "  Cat" if $chain;
  print "\n";
  foreach (sort { $files{$b}{start} <=> $files{$a}{start} } keys %files)
  {
    next unless $files{$_}{name};
    my $n=$files{$_}{name} . (" "x10); $n=substr($n,0,10);
    printf "%10s  %s  %06X %06X %06X %03X",
                          $n,
                          ($files{$_}{locked}?"L":" "),
                          $files{$_}{load},
                          $files{$_}{exec},
                          $files{$_}{size},
                          $files{$_}{start};
    printf("  %03X",$files{$_}{cat_sector}) if $chain;
    print "\n";
  }
}

# Delete a file from a catalogue
sub delete_file($$$)
{
  my ($force,$filename,$image)=@_;
  if (!$force)
  {
    print "Delete? $filename (Y/N) ";
    my $x=<STDIN>;
    return unless $x=~/^Y/i;
  }
  my %cat=read_cat($image);

  foreach (0..$CatalogueMaxFiles-1)
  {
    if ($filename eq $cat{$_}{name})
    {
      my $start=8+8*$_;
      my $len=256-$start;
      substr($$image,$start,$len)=substr($$image,$start+8,$len);
      substr($$image,256+$start,$len)=substr($$image,256+$start+8,$len);
      substr($$image,248,8)="\0" x 8;
      substr($$image,256+248,8)="\0" x 8;
      my $nfiles=ord(substr($$image,256+5,1))-8;
      substr($$image,256+5,1)=chr($nfiles);
      print "Deleted $filename\n" if $force;
      last;
    }
  }
}

# Does the given filename match a wildcard(array)
sub filename_compare($@)
{
  my ($filename,@matches)=@_;

  foreach (@matches)
  {
    my $x=$_;
    $x = '$.' . $x unless $x=~/^.\./;
    # Convert BBC filepattern to regex
    $x=~s/\./\\./g;
    $x=~s/#/./g;
    $x=~s/\*/.*/g;
    $x=~s/\$/\\\$/g;
#    print "Compare $filename to $x\n";
    return 1 if ($filename =~ /^$x$/i);
  }
  return 0;
}

sub delete_files($$@)
{
  my ($force,$image,@fspec)=@_;
  
  my %files=BeebUtils::read_cat($image);

  die "Can not operate on large/multi-catalogue disks\n" if $files{""}{chain};
  delete $files{""};
  
  # Make a hash-tree out of the files we want to delete
  my %to_del;
  foreach (@fspec)
  {
    # Put files into "$." if no library specified
    $to_del{$_}=1;
  }

  my $matched=0;
  foreach (keys %files)
  {
    if (filename_compare($files{$_}{name},keys %to_del))
    {
      $matched=1;
      if ($files{$_}{locked})
      {
        print "$files{$_}{name} Locked\n";
      }
      else
      {
        delete_file($force,$files{$_}{name},$image);
      }
    }
  }

  if ($matched)
  {
    write_ssd($image,$BBC_FILE);
  }
  else
  {
    print "No files matched\n" unless $matched;
  }
}

sub lock_files($$$)
{
  my ($filename,$lock,$image)=@_;
  
  my %files=BeebUtils::read_cat($image);

  die "Can not operate on large/multi-catalogue disks\n" if $files{""}{chain};
  delete $files{""};

  my $matched=0;
  foreach (keys %files)
  {
    if (filename_compare($files{$_}{name},($filename)))
    {
      my $offset=$_*8+8+7;
      my $dir=ord(substr($$image,$offset,1));
      if ($files{$_}{locked} && !$lock)
      {
        substr($$image,$offset,1)=chr($dir & 127);
        $matched=1;
      }
      elsif ($lock && !$files{$_}{locked})
      {
        substr($$image,$offset,1)=chr($dir | 128);
        $matched=1;
      }
    }
  }

  if ($matched)
  {
    write_ssd($image,$BBC_FILE);
  }
  else
  {
    print "No files matched\n" unless $matched;
  }
}

sub compact_ssd($)
{
  my ($image)=@_;
  my %files=read_cat($image);
  die "Can not operate on large/multi-catalogue disks\n" if $files{""}{chain};
  die "Can only operate on 200Kb disks\n" if $files{""}{disk_size} != 0x320;
  my $cnt=$files{""}{filecount}-1;
  delete $files{""};

  my $freesect=2;
  my $changed=0;
  foreach (reverse (0..$cnt))
  {
    my $thissect=$files{$_}{start};
    my $len=$files{$_}{size};
    # Convert to sectors;
    $len=int($len/256)+(($len%256)?1:0);
    
    if ($thissect != $freesect)
    {
##      printf "Moving $files{$_}{name} (%04X) from %03X to %03X\n",$len,$thissect,$freesect;
      substr($$image,$freesect*256,$len*256)=substr($$image,$thissect*256,$len*256);
      
      # Now we need calculate the new byte6/7 for this entry.  We
      # know this is a 200Kb disk and so Acorn standard, so no complications
      # from Solidisk!
      my $b6=ord(substr($$image,256+8+$_*8+6,1)) & 0xfc;
      $b6 |= ($freesect >> 8);
      my $b7=$freesect & 255;
      substr($$image,256+8+$_*8+6,2)=chr($b6).chr($b7);
      $changed=1;
    }
    $freesect+=$len;
  }
  return ($changed);
}

sub rename_file($$$)
{
  my ($image,$old_name,$new_name)=@_;
  my %files=read_cat($image);
  die "Can not operate on large/multi-catalogue disks\n" if $files{""}{chain};
  die "Can only operate on 200Kb disks\n" if $files{""}{disk_size} != 0x320;
  die "Names are same\n" if $old_name eq $new_name;

  $old_name = '$.' . $old_name unless $old_name=~/^.\./;
  $new_name = '$.' . $new_name unless $new_name=~/^.\./;
  $old_name=substr($old_name,0,9);
  $new_name=substr($new_name,0,9);

  my $cnt=$files{""}{filecount}-1;
  my $changed=0;

  # does the new name already exist?
  foreach (0..$cnt)
  {
    die "File already exists\n" if lc($files{$_}{name}) eq lc($new_name);
  }

  foreach (0..$cnt)
  {
    next unless $files{$_}{name} eq $old_name;
    
    die "File locked\n" if $files{$_}{locked};

    my $name=substr($new_name . "       ",2,7);  # skip directory
    my $dir=substr($new_name,0,1);

    substr($$image,8+$_*8,8)=$name . $dir;
    $changed=1;
  }

  if ($changed)
  {
    write_ssd($image,$BBC_FILE);
  }
  else
  {
    print "File not found.\n";
  }
}

sub opt4($$)
{
  my ($image,$val)=@_;

  # We mangle byte 0x106
  my $b=0x106;

  # Existing value
  my $v=ord(substr($$image,$b,1));

  # Mask out old value
  $v &= 0xCF;

  # Add new value
  $v |= ($val << 4);

  # Update image
  substr($$image,$b,1)=chr($v);
}

sub check_and_set_type($)
{
  my ($type)=@_;

  if ($type =~ /solidisk|stl/i) { $SOLIDISK=1; }
  elsif ($type =~ /watford/i) { $WATFORD=1; }
  elsif ($type =~ /opus|ddos/i) { $OPUS=1; }
  elsif ($type =~ /diskdoctor|discdoctor/i) { $DISK_DOCTOR=1; }
  elsif ($type !~ /acorn/i) { print "Unknown DFS $type\n"; exit(-1); }
}

my $dfs=$ENV{BEEB_UTILS_DFS};

if (!defined($dfs) && defined($ENV{HOME}) && -f "$ENV{HOME}/.beeb_utils_dfs")
{
  my $fh=new FileHandle "<$ENV{HOME}/.beeb_utils_dfs";
  if ($fh)
  {
    $dfs=<$fh>;  chomp($dfs);
  }
}

if ($dfs)
{
  $dfs="acorn:$dfs" unless $dfs=~/:/;
  my ($type,$file)=split(/:/,$dfs);
  $DEFAULT_BBC_FILE=$file if $file;
  check_and_set_type($type);
}

our $PROG=$0; $PROG=~s!^.*/!!; $PROG=~s/\.pl$//;

1;
