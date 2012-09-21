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
my $describe    = undef;                  # Describe the selected output format.
my $output_file = undef;                  # Output GPX to this file.
my $input_file  = undef;                  # Org-file to reead.
my $track_name  = undef;                  # Match exact name.
my $track_len   = undef;                  # Try to find the len in track's name.
my $headline    = undef;                  # Restrict to headline.
my $subtree     = undef;                  # Restrict to all tracks in subtree.
my $help        = undef;                  # Show help?
my $info        = undef;                  # Just print informations about
                                          # tracks.

Getopt::Long::Configure ("gnu_getopt");
my $result = GetOptions (
    "out|output|o=s"    => \$output_file,
    "in|input|i"        => \$input_file,
    "describe|desc=s"   => \$describe,
    "format=s"          => \$format,
    "min|len=i"         => \$track_len,
    "name|tn|n=s"       => \$track_name,
    "heading=s"         => \$headline,
    "subtree=s"         => \$subtree,
    "help|h"            => \$help,
    "info|inf"          => \$info,
    );


if($help) {
    help();
    exit 0;
}
if($info) {
    $format = "info";
}




if($describe) {
    my $formatter = Formatter::ForName(undef, $describe);
    $formatter->describe();
    exit 0;
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


$format ||= 'simple';
my $formatter = Formatter::ForName($OUT, $format);

$formatter->leadIn();


while( <$IN> )
{
    last if $max_offset && $max_offset <= tell($IN);

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


sub ForName {                                     # static
    my($fh, $format) = @_;
    die "No output format given." unless $format;
    given( $format ) {
        when(/gpx/i) {
            return GPXFormatter->new($OUT, 'GPX');
        }
        when(/info/i) {
            return INFOFormatter->new($OUT, 'INFO');
        }
        default {
            return SIMPLEFormatter->new($OUT, 'SIMPLE');
        }
    }
}

sub new {
    my($class, $file_handle, $format_name) = @_;
    bless {
        FH => $file_handle,
        format_name => $format_name,
    }, shift;
}

sub leadIn {
    die ("leadIn(): NOT IMPLEMENTED for " . uc($_[0]->{format_name}));
}

sub writeTrack {
    die ("writeTrack(): NOT IMPLEMENTED for " . uc($_[0]->{format_name}));
}

sub leadOut {
    die ("leadOut(): NOT IMPLEMENTED for " . uc($_[0]->{format_name}));
}

sub describe {
    my $self = shift;
    print "Output format\n", " " x 35, $self->{format_name}, "\n\n";
    print $self->getDescription, "\n";
}

sub getDescription {
    die ("getDescription(): NOT IMPLEMENTED for " . uc($_[0]->{format_name}));
    # See SIMPLEFormatter for an example.
}

1;



package SIMPLEFormatter;

use base "Formatter";

sub new {
    my $class = shift;
    bless
        $class->SUPER::new( @_ ),
        $class;
}

sub leadIn {
    my $self = shift;
    print { $self->{FH} } "# All coordinates as pairs of 'longitude latitude'\n";
}

sub writeTrack {
    my($self, $track_name, $long_lat) = @_;
    print { $self->{FH} } "\n* $track_name\n";
    for( @{$long_lat} ) {
        print { $self->{FH} } "$_\n";
    }
}

sub leadOut {
    # Nothing to do here.
}

sub getDescription {
    return "\t"
        . join("\n\t",
               ("Prints all the tracks requested seperated by empty lines.",
                "Each track consists of a name, which is printed first.",
                "The name line starts with an asterisk('*').",
                "The rest of the track consists of lines each of which prints",
                "a pair of coordinates separated by a space.",
               ));
}

1;



package INFOFormatter;

use base "Formatter";

sub new {
    my $class = shift;
    my $self = bless
        $class->SUPER::new( @_ ),
        $class;
    $self->{head_format} = "| %-34.34s | %7s | %21s | %21s | %21s | %21s |\n";
    $self->{data_format} = "| %-34.34s | %7s | %21s | %21s | %21s | %21s |\n";
    $self->{hline} = sprintf(
        "+%36s+%9s+%23s+%23s+%23s+%23s+\n",
        "-" x 36,
        "-" x 9,
        "-" x 23, "-" x 23, "-" x 23, "-" x 23
        );
    $self->{headline} = sprintf $self->{head_format},
    "Name", "Coords",
    "Farthest North", "Farthest East", "Farthest South", "Farthest West";
    $self->{n} =  -90.0;
    $self->{s} =   90.0;
    $self->{e} = -360.0;
    $self->{w} =  360.0;
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
    $track_name,
    scalar(@{$long_lat}),
    $self->fill($n), $self->fill($e), $self->fill($s), $self->fill($w);
    $self->{points} += scalar(@{$long_lat});
}

sub leadOut {
    my $self = shift;
    print { $self->{FH} } $self->{hline};
    printf { $self->{FH} } $self->{data_format},
    "Total:", $self->{points},
    $self->fill($self->{n}),
    $self->fill($self->{e}),
    $self->fill($self->{s}),
    $self->fill($self->{w});
}

sub fill {
    # Fill choords with spaces to the right.
    my $val = pop;
    my $factor = 18 - ( length($val) - index($val, '.') );
    if(0 < $factor) {
        $val .= substr("                   ", 0, $factor);
    }
    return $val;
}

sub getDescription {
    return "\t"
        . join("\n\t",
               ("Prints a table with the name and the max north east south west",
                "coords of each track found.",
                "The last line shows the maximum found for the cardinal points.",
               ));
}

1;




package GPXFormatter;

use base "Formatter";

sub new {
    my $class = shift;
    bless
        $class->SUPER::new( @_ ),
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

sub getDescription {
    return "\t"
        . join("\n\t",
               ("Writes a GPX file to the selected output (STDOUT by default).",
                "All selected tracks are written to that single GPX file.",
               ));
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
        --help | -h             Show this help and exit.

        --format | -f  FORMAT   Choose output format.  Supported is the default
                                'simple' textformat 'gpx' and an 'info' table.
                                Format names are case insensitive.

        --output | -o FILENAME  Write Tracks to file FILENAME.  Dies if FILENAME
                                already exists.

        --info | -i             Just print inforamtions about the tracks found
                                such as name, farthes point north, east south
                                and west.
        --describe  FORMAT      Describe output format FORMAT.

AUTHOR:
                   Author and Copyright © 2011-2012 Sebastian Rose
