#!/usr/bin/perl -w

use strict;
use utf8;
use Getopt::Long;
use File::Basename;
use IO::Select;
use feature "switch";
use Fcntl qw(:seek);
no warnings qw/experimental/;

my $program_name = basename($0);


## Options:
my $format      = undef;                  # Output format.
my $describe    = undef;                  # Describe the selected output format.
my $output_file = undef;                  # Output GPX to this file.
my $input_file  = undef;                  # Org-file to reead.
my $force       = undef;                  # Overwrite existing file.
my $track_name  = undef;                  # Match exact name.
my $track_len   = undef;                  # Try to find the len in track's name.
my $subtree     = undef;                  # Restrict to all tracks in subtree.
my $max_tracks  = undef;                  # Limit number of tracks.
my $help        = undef;                  # Show help?
my $info        = undef;                  # Just informations about tracks.
my $debug       = undef;                  # Print debugging information.

Getopt::Long::Configure ("gnu_getopt");
my $result = GetOptions (
    "out|output|o=s"    => \$output_file,
    "in|input|i=s"      => \$input_file,
    "describe|desc=s"   => \$describe,
    "format|f=s"        => \$format,
    "force"             => \$force,
    "min|len=i"         => \$track_len,
    "name|tn|n=s"       => \$track_name,
    "max|m=i"           => \$max_tracks,
    "subtree|s=s"       => \$subtree,
    "help|h"            => \$help,
    "info|inf"          => \$info,
    "debug"             => \$debug,
    );

unless($result) {
    print "Consult `$program_name -h'\n";
    exit 1;
}
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
if($input_file) {
    open($IN, "<", $input_file) or die $!;
}
else {
    my $in = IO::Select->new();
    $in->add(\*STDIN);
    if($in->can_read(0)) {
        $IN = \*STDIN;
    }
    elsif(0 < scalar(@ARGV)) {
        $input_file = pop;
        open($IN, "<", $input_file) or die $!;
    }
}
die "No data on STDIN and no input file given.  `$program_name -h' might help.\n" unless $IN;



## Narrow to region:
my $min_offset  = 0;
my $max_offset  = 0;



## Restrict to a subtree.
if($subtree) {

    my $line_start = "*";
    my $old_line_start = undef;
    my $line;

    if($subtree =~ /^\d+(\.\d+)*$/)               # Section number given?
    {
        my @secs = split(/\./, $subtree);
        my $sec  = shift @secs;
        my $found = 0;

        ## Search beginning of subtree.
        while($line = <$IN>) {

            if($old_line_start && 0 == index($line, "$old_line_start ")) {
                logDebug("$line");
                die "    => ERROR: no deeper nested level found after position $min_offset!\n";
            }

            if(0 == index($line, "$line_start "))
            {
                ++$found;
                logDebug("$line");
            }

            if($found == $sec)                    # Section found.
            {
                if(0 == scalar(@secs)) {          # Final Section found.
                    $min_offset = tell($IN);      # Set min_offset
                    {
                        use bytes;
                        $min_offset -= length($line);
                    }
                    # Remember the correct level.  We will need this below, when
                    # we search for the end of our subtree.
                    $line_start = $old_line_start;
                    last;                         # and quit this search.
                }

                $found = 0;                       # Reset
                $sec = shift @secs;               # and search next deeper nested level.
                $min_offset = tell($IN);          # Remember beginning of line
                                                  # following last match.

                while($line = <$IN>) {            # Search for nested level.
                    if(0 == index($line, "$line_start")) {
                        my @m = $line =~ /^(\*+)/;
                        unless(length($m[0]) > length($line_start)) {
                            logDebug($line);
                            die "    => ERROR: no deeper nested level found after position $min_offset!\n";
                        }
                        $old_line_start = $line_start;
                        $line_start = $m[0];      # Regardless of odd/even.
                         # Seek back to beginning of line following last match:
                        seek($IN, $min_offset, SEEK_SET);
                        last;
                    }
                    continue;
                }
            }
        }
    }
    ##  END OF     if( numeric subtree ... )

    else
    {
        $subtree =~ s/^\**\s*//;
        logDebug("Searching for Headline '$subtree'\n");
        while($line = <$IN>) {
            if($line =~ /^(\*+)\s*$subtree$/) {
                # Again: remember the correct level.  We will need this below,
                # when we search for the end of our subtree.
                $line_start = $1;
                logDebug($line);
                $min_offset = tell($IN);
                {
                    use bytes;
                    $min_offset -= length($line);
                }
                last;
            }
        }
    }
    ## END OF     if( ! numeric subtree )

    unless($min_offset > 0) {
        die "'$subtree':  section not found.\n";
    }

    logDebug("Section $subtree found at (byte-) offset $min_offset\n");

    ## In either case, search end of subtree:
    seek($IN, $min_offset, SEEK_SET);
    <$IN>;                                        # Skip our headline.
    while($line = <$IN>) {
        if($line =~ /^(\*+)/) {
            if(length($1) <= length($line_start)) {
                {
                    use bytes;
                    $max_offset = tell($IN) - length($line);
                }
                logDebug( "End of subtree at (byte-) offset $max_offset.\n",
                          "At beginning of this line:\n",
                          $line );
                last;
            }
        }
    }

}
## END OF   if( $subtree )



my $OUT = \*STDOUT;                               # Print everything to this filehandle;
if($output_file) {
    # 1. Open $output_file
    if(-f $output_file) {
        die "'$output_file': File already exists!\n" unless $force;
        logDebug("Overriding existing output file $output_file\n");
    }
    open(OUT, ">", $output_file) or die $!;
    # 2. $OUT = *output_file_handle.
    $OUT = \*OUT;
}


$format ||= 'simple';
my $formatter = Formatter::ForName($OUT, $format);

$formatter->leadIn();
my $tracks_extracted = 0;
seek($IN, $min_offset, SEEK_SET);
while( <$IN> )
{
    last if $max_offset && $max_offset <= tell($IN);
    last if $max_tracks && $max_tracks <= $tracks_extracted;

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
        /iox )
    {
        $name = basename $2;
        @long_lat = split(/\s*\)\s*\(\s*/o, $1);
        # Cut of the remaining parens:
        $long_lat[0] =~ s/\(\s*//;
        $long_lat[$#long_lat] =~ s/\s*\)//;
        $formatter->writeTrack($name, \@long_lat);
        $tracks_extracted++;
    }
}

$formatter->leadOut();


close $OUT or die $!;
close $IN  or die $!;

exit 0;


sub logDebug {
    if ($debug) {
        print STDERR @_;
    }
}


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
        when(/^gpx$/i) {
            return GPXFormatter->new($OUT, 'GPX');
        }
        when(/^info$/i) {
            return INFOFormatter->new($OUT, 'INFO');
        }
        when(/^svg$/i) {
            return SVGFormatter->new($OUT, 'SVG');
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





package SVGFormatter;

use base "Formatter";
use Math::Trig;

# Package defaults:
my $default_zoom   = 15;
my $default_margin = 10;
my $default_cache_directory = $ENV{HOME} . "/.emacs.d/osm";

sub new {
    my $class = shift;
    my $self = $class->SUPER::new( @_ );
    bless $self, $class;

    $self->{osmMaxLatitude} = 85.05112877;

    unless( defined($self->{params}{tiles}) && $self->{params}{tiles} ) {
        main::logDebug( "No BG-Tiles directory given. Using default: "
            # . $SVGFormatter::default_cache_directory
            . $ENV{HOME} . "/.emacs.d/osm"
            . "\n" );
        $self->{params}{tiles} = $ENV{HOME} . "/.emacs.d/osm"; # $SVGFormatter::default_cache_directory;
    }
    unless(-d $self->{params}{tiles}) {
        die $self->{params}{tiles} . ": No such directory.";
    }

    unless( defined($self->{params}{zoom}) && $self->{params}{zoom} ) {
        main::logDebug( "No zoom level given. Using default: "
            # . $SVGFormatter::default_zoom
            . "15"
            . "\n" );
        $self->{params}{zoom} = 15; # $SVGFormatter::default_zoom;
    }
    $self->_setZoom($self->{params}{zoom});
    $self->{done} = 0;
    $self;
}

sub _setZoom {
    my($self, $z) = @_;
    $self->{zoom} = int($z);
    $self->{circInTiles}  = 1 << $self->{zoom};   # Circumference in tiles.
    $self->{circInPixels} =                       # Circumference in pixels.
        $self->{circInTiles} << 8;
    $self->{greenwich}    =                       # The Prime meridian's distance
        $self->{circInPixels} >> 1;               # from the Antimeridian in pixels.
    $self->{equator}      =                       # Equator in pixels from top.
        128 << $self->{zoom};
    $self->{radiusInPixels} = $self->{circInPixels} / pi;
    $self->{margin} = $self->{zoom} * 5;          # Margin in Pixels.
}

sub leadIn {
    my $self = shift;
    print { $self->{FH} }
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n",
    "<!DOCTYPE svg PUBLIC \"-//w3c//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n";
}

#
# TODO:
#  - Make (the-farthest-west-coor MINUS $minx) = 0.
#  - Record each track as a <g> element and remember their upper left and lower right corners (plus coords).
#  - Fill the svg with background tiles as needed.
#  - Place the <g> elements in an SVG (on leadOut()) big enough to hold them all plus some margin.
#
sub writeTrack {
    my($self, $track_name, $long_lat) = @_;
    if($self->done()) {
        main::logDebug( "SVG-Format: only one track at a time. Sorry.\n" );
        return;
    }
    my $old_fh = select($self->{FH});
    main::logDebug( "Drawing $track_name ONLY.\n" );
    my $minx = 360.0;                             # Store mininum West
    my $maxx =   0.0;                             # Store maximum West
    my $miny =  90.0;                             # Store minimum North
    my $maxy =   0.0;                             # Store maximum North

    for( @{$long_lat} ) {
        my($long, $lat) = split(" ");
        if($long < $minx) { $minx = $long; }
        if($long > $maxx) { $maxx = $long; }
        if($lat  < $miny) { $miny = $lat; }
        if($lat  > $maxy) { $maxy = $lat; }
    }
    # Translate degrees to pixels:
    $minx = $self->_longitudeToX($minx);
    $maxx = $self->_longitudeToX($maxx);
    $miny = $self->_latitudeToY($miny);
    $maxy = $self->_latitudeToY($maxy);
    main::logDebug( "In pixels => minx maxx miny maxy: %s %s %s %s\n", $minx, $maxx, $miny, $maxy );
    # Swap y values (southern hemisphere yield negative latitudes):
    if($maxy < $miny) {
        my $tmp = $maxy;
        $maxy = $miny;
        $miny = $tmp;
    }
    main::logDebug( "Evtl. swap y => minx maxx miny maxy: %s %s %s %s\n", $minx, $maxx, $miny, $maxy );
    # Add the margin:
    $minx -= $self->{margin};
    $maxx += $self->{margin};
    $miny -= $self->{margin};
    $maxy += $self->{margin};
    main::logDebug( "Margin added => minx maxx miny maxy: %s %s %s %s\n", $minx, $maxx, $miny, $maxy );

    # # Substract  get the tiles we need (rows and columns):
    my $cmin = $minx >> 8;
    my $cmax = $maxx >> 8;
    my $rmin = $miny >> 8;
    my $rmax = $maxy >> 8;
    main::logDebug( "cmin cmax rmin rmax: %s %s %s %s\n", $cmin, $cmax, $rmin, $rmax );
    # Calculate the margines.  The two margins now are positive integers to
    # substract from all x and y coords.
    my $mtop  = $miny - ($rmin << 8);
    my $mleft = $minx - ($cmin << 8);
    main::logDebug( "mleft mtop: %s %s\n", $mleft, $mtop );

    # calculate width and height:
    my $width  = $maxx - $minx;
    my $height = $maxy - $miny;

    print "<svg xmlns=\"http://www.w3.org/2000/svg\"
        xmlns:xlink=\"http://www.w3.org/1999/xlink\"
        version=\"1.1\"
        width=\"$width\" height=\"$height\">\n",
    " <g id=\"layer1\">\n";

    # BACKGROUND TILES....

    # Draw the track itself:
    my($long, $lat) = split(" ", shift(@{$long_lat}));
    print
        "  <path\n",
        "    d=\"M ",
            ( $self->_longitudeToX($long) - $minx),
            ",",
            ( $self->_latitudeToY($lat) - $miny),
        " L";
    for( @{$long_lat} ) {
        ($long, $lat) = split(" ");
        print
            " ",
            ( $self->_longitudeToX($long) - $minx),
            ",",
            ( $self->_latitudeToY($lat) - $miny),
    }

    print "\"\n",
    "    style=\"fill:none;stroke:#ff0000;stroke-width:4;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-opacity:0.95;stroke-dasharray:none\"",
    " />\n",
    " </g>\n";
        # "</svg>\n";
    select($old_fh);
    $self->done(1);
}

sub done {
    my($self, $done) = @_;
    $self->{done} = $done   if $done;
    $self->{done};
}

sub leadOut {
    my $self = shift;
    print { $self->{FH} } "</svg>\n";
}

sub getDescription {
    return "\t"
        . join("\n\t",
               ("Writes a SVG file to the selected output (STDOUT by default).",
                "All selected tracks are written to that single SVG file.",
                "",
                "Required Parameters:",
                "NOT  IMPLEMENTED  YET.",
               ));
}

# Return the x value in pixels from the date line for a certain latitude.
sub _longitudeToX {
    my($self, $longitude) = @_;
    return int( $self->{greenwich}
                + ( ($self->{circInPixels} / 360) * $longitude )
        );
}

# Return the y value in pixels (offset) from the northpole and the
# y-number of the tile that pixel is found on as [ pixel tileY ].
sub _latitudeToY {
    my( $self, $latitude ) = @_;

    main::logDebug( "Latitude: $latitude\n" );
    main::logDebug( "Äquator: $self->{equator}\n" );

    if( abs($latitude) >= $self->{osmMaxLatitude} )
    {
        $latitude = $self->{osmMaxLatitude};
        if( $latitude > 0) {
            main::logDebug( "Latitude ", $latitude, " out of bounds! Using ",
                            $self->{osmMaxLatitude}, ", i.e. the maximum.\n" );
        }

        else {
            main::logDebug("Latitude ", $latitude, " out of bounds! Using -",
                           $self->{osmMaxLatitude}, ", i.e. the minimum.\n" );
            $latitude *= -1.0;
        }
    }

    my $z = (0 < ($self->{zoom} - 1))
        ? $self->{zoom} - 1
        : -1.0 * $self->{zoom};
    my $lat = deg2rad( abs $latitude );
    my $ret = ($self->{radiusInPixels} / 2.0) *
        log( (1.0 + sin($lat))
             /
             (1.0 - sin($lat))
        );

    if($latitude >= 0) {
        return int($self->{equator} - $ret);
    }
    return int($self->{equator} + $ret);
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
        --help | -h
                Show this help and exit.

        --format FORMAT, -f  FORMAT
                Choose output format.  Supported is the default 'simple'
                textformat 'gpx' and an 'info' table. Format names are case
                insensitive.

        --output FILENAME, --out FILENAME, -o FILENAME
                Write Tracks to file FILENAME.  Dies if FILENAME already exists.

        --input FILENAME, --in FILENAME, -i FILENAME
                Read Tracks from FILENAME.

        --info, --inf
                Just print inforamtions about the tracks found such as name,
                farthes point north, east south and west.

        --debug
                Print debugging information to STDERR.

        --describe  FORMAT
                Describe output format FORMAT.

        --subtree | -s SECTION
                Restrict extraction to tracks found in a certain subtree.
                SECTION may be the exact heading or a numeric section identifier
                like '2.1'.

        --max | n MAX
                Limit the number of tracks to MAX (integer).

AUTHOR:
                   Author and Copyright © 2011-2012 Sebastian Rose
