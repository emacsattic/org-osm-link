#!/usr/bin/perl -w

use strict;
use utf8;
use Getopt::Long;
use File::Basename;
use IO::Select;
use feature "switch";


my $program_name = basename($0);


## Options:
my $format      = undef;                  # Output format.
my $output_file = undef;                  # Output GPX to this file.
my $track_name  = undef;                  # Match exact name.
my $track_len   = undef;                  # Try to find the len in track's name.
my $headline    = undef;                  # Restrict to headline.
my $subtree     = undef;                  # Restrict to all tracks in subtree.
my $help        = undef;                  # Show help?
my $info        = undef;                  # Just print informations about
                                          # tracks.

my $result = GetOptions (
    "out|output|o=s"    => \$output_file,
    "format=s"          => \$format,
    "min|len=i"         => \$track_len,
    "name|tn|n=s"       => \$track_name,
    "heading=s"         => \$headline,
    "subtree=s"         => \$subtree,
    "help|h"            => \$help,
    "info|i"            => \$info,
    );


if($help) {
    help();
    exit 0;
}
if($info) {
    $format = "info";
}



my $IN = undef;
if(0 < scalar(@ARGV)) {
    my $input_file = pop;
    open($IN, "<", $input_file) or die $!;
}
else {
    my $in = IO::Select->new();
    $in->add(\*STDIN);
    if($in->can_read(0)) {
        $IN = \*STDIN;
    }
}
die "No data on STDIN and no input file given." unless $IN;


## Variables
my $min_offset  = 0;
my $max_offset  = 0;


## TODO: Restrict to a subtree.
if($subtree) {
    # 1. Read to start of subtree and set $min_offset accordingly.
    # 2. Find end of subtree and set $max_offset accordingly.
    # 3. Seek back to $min_offset.
}

## TODO: Restrict to a headline (possibly found in subtree).
if($headline) {
    # 1. Find $head_line and set $min_offset accordingly.
    # 2. Find next headline and set $max_offset accordingly.
    # 3. Seek back to $min_offset.
}


my $OUT = \*STDOUT;                               # Print everything to thes filehandle;
if($output_file) {
    # 1. Open $output_file
    die "'$output_file': File already exists!\n"  if -f $output_file;
    open(OUT, ">", $output_file) or die $!;
    # 2. $OUT = *output_file_handle.
    $OUT = \*OUT;
}


my $formatter = undef;
given( $format ) {
    when('gpx') {
        $formatter = GPXFormatter->new($OUT);
    }
    when('info') {
        $formatter = INFOFormatter->new($OUT);
    }
    default {
        $formatter = Formatter->new($OUT);
    }
}
$formatter->leadIn();


while( <$IN> )
{
    last if $max_offset && $max_offset <= tell($IN);

    # print "."; Je mehr Punkte, desto länger die Beschreibung...

    my($name, @long_lat);
    if( /^\s*\[\[                                 # Start of link
         track:
         \s*[^\(]*                                # Filename preceeding coords
         \s*\(                                    # Paren starting coords
         ([^\]]*)                                 # Pairs of (long lat)
         \s*\)                                    # Paren ending coords
         \s*[^\)]*                                # Filename following coords
         \]                                       # End of first link part
         \[                                       # Start second link part
         ([^\]]*)                                 # Name of the track
         \]\]                                     # End of link
        /iox ) {
        $name = basename $2;
        @long_lat = split(/\s*\)\s*\(\s*/o, $1);
        # Cut of the remaining parens:
        $long_lat[0] =~ s/\(\s*//;
        $long_lat[$#long_lat] =~ s/\s*\)//;
        # print $name, "\n";
        # print join(",\n", @long_lat);
        $formatter->writeTrack($name, \@long_lat);
    }
}

$formatter->leadOut();


close $OUT or die $!;
close $IN  or die $!;

exit 0;




sub trim
{
	$_[0] =~ s/^[\s\f\n\r\t]*//o;
	$_[0] =~ s/[\s\f\n\r\t]*$//o;
	$_[0];
}


sub help {
    while(<DATA>) {
        s/%program_name%/$program_name/og;
        print;
    }
}



package Formatter;

sub new {
    my($self, $file_handle) = @_;
    bless {
        name => 'simple',
        FH => $file_handle
    }, shift;
}

sub leadIn {
    my $self = shift;
    print { $self->{FH} } "== Coordinates (long lat) ==\n";
}

sub writeTrack {
    my($self, $track_name, $long_lat) = @_;
    print { $self->{FH} } "* $track_name\n";
    for( @{$long_lat} ) {
        print { $self->{FH} } "$_\n";
    }
}

sub leadOut {
    my $self = shift;
    print { $self->{FH} } "DONE\n";
}

1;



package INFOFormatter;

use base "Formatter";

sub new {
    my($class, $file_handle) = @_;
    my $self = $class->SUPER::new($file_handle);
    $self->{data_format} = "| %-32s | %7s | %-21s | %-21s | %-21s | %-21s |\n";
    $self->{hline} = sprintf(
        "+%-34s+%9s+%23s+%23s+%23s+%23s+\n",
        "-" x 34,
        "-" x 9,
        "-" x 23, "-" x 23, "-" x 23, "-" x 23
        );
    $self->{headline} = sprintf $self->{data_format},
    "Name", "Coords",
    "Farthest North", "Farthest East", "Farthest South", "Farthest West";
    $self->{n} = -90;
    $self->{s} = 90;
    $self->{e} = -360.0;
    $self->{w} = 360.0;
    $self->{points} = 0;
    bless $self, $class;
}

sub leadIn {
    my $self = shift;
    printf { $self->{FH} } $self->{headline};
    print { $self->{FH} } $self->{hline};
}

sub writeTrack {
    my($self, $track_name, $long_lat) = @_;
    my $n = -90;
    my $s = 90;
    my $e = -360.0;
    my $w = 360.0;
    for(@{$long_lat}) {
        my($long, $lat) = split(" ");
        if($long < $w) { $w = $long; }
        if($long > $e) { $e = $long; }
        if($lat  < $s) { $s = $lat; }
        if($lat  > $n) { $n = $lat; }
        # And the global minimums and maximums:
        if($long < $self->{w}) { $self->{w} = $long; }
        if($long > $self->{e}) { $self->{e} = $long; }
        if($lat  < $self->{s}) { $self->{s} = $lat; }
        if($lat  > $self->{n}) { $self->{n} = $lat; }
    }
    printf { $self->{FH} } $self->{data_format},
    $track_name, scalar(@{$long_lat}), $n, $e, $s, $w;
    $self->{points} += scalar(@{$long_lat});
}

sub leadOut {
    my $self = shift;
    print { $self->{FH} } $self->{hline};
    printf { $self->{FH} } $self->{data_format},
    "Total:", $self->{points}, $self->{n}, $self->{e}, $self->{s}, $self->{w};
}

1;



package GPXFormatter;

use base "Formatter";

sub new {
    my($class, $file_handle) = @_;
    bless
        $class->SUPER::new($file_handle),
        $class;
}

sub leadIn {
    my $self = shift;
    print { $self->{FH} }
    "<?xml version=\"1.0\"?>\n",
    "<gpx version=\"1.0\" creator=\"$program_name\"\n",
    "     xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n",
    "     xmlns=\"http://www.topografix.com/GPX/1/0\"\n",
    "     xsi:schemaLocation=\"http://www.topografix.com/GPX/1/0 http://www.topografix.com/GPX/1/0/gpx.xsd\">\n";
}

sub writeTrack {
    my($self, $track_name, $long_lat) = @_;
    my $old_fh = select($self->{FH});
    print
        " <trk>\n",
        "  <name>$track_name</name>\n",
        "  <trkseg>\n";
    for( @{$long_lat} ) {
        my($long, $lat) = split(" ");
        print "   <trkpt lat=\"$lat\" lon=\"$long\"></trkpt>\n";
    }
    print "  </trkseg>\n",
    " </trk>\n";
    select($old_fh);
}

sub leadOut {
    my $self = shift;
    print { $self->{FH} } "</gpx>\n";
}

1;



package main;

__DATA__

NAME:   %program_name%

USAGE:
        %program_name% [ -o FILENAME ] [ -f FORMAT ] [ -i ] filename.org

        cat filename.org | %program_name%

DESCRIPTION:
        Extract Tracks from org-osm links and export them to a certain format.
        The default format is a simple text format and the default target is
        STDOUT.

OPTIONS:
        -help | -h              Show this help and exit.

        -format | -f  FOMRAT    Choose output format.  Supported is the default
                                simple textformat and GPX.  Case insensitive.

        -output | -o  FILENAME  Write Tracks to file FILENAME.  Dies if FILENAME
                                already exists.

        -info | -i              Just print inforamtions about the tracks found
                                such as name, farthes point north, east south
                                and west.

AUTHOR:
                   Author and Copyright © 2011-2012 Sebastian Rose
