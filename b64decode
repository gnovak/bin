#!/usr/local/bin/perl
# b64decode -- decode a raw BASE64 message
# A P Barrett (barrett@ee.und.ac.za), October 1993

# usage: b64decode [files]

# Input is read from stdin or from the named files.  Each named file
# is processed separately.  The input should be raw base64-encoded data,
# without any heades, trailers or other junk (so remove that first,
# before using b64decode).  See RFC-1341 for the definition of
# base64 encoding.
# Output is the decoded message, and is sent to stdout with 8 data bits
# per output character.

# The present implementation is horribly inefficient.

$base64alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
                  'abcdefghijklmnopqrstuvwxyz'.
                  '0123456789+/'; # and '='
$base64pad = '=';

$leftover = '';
while (<>) {
    # ignore illegal characters
    s/[^$base64alphabet]//go;
    # insert the leftover stuff from last time
    $_ = $leftover . $_;
    # if there are not a multiple of 4 bytes, keep the leftovers for later
    m/^((....)*)/; $_=$&; $leftover=$';
    # turn each group of 4 values into 3 bytes
    s/(....)/&b64decodesub($1)/eg;
    # special processing at EOF for last few bytes
    if (eof) {
	$_ .= &b64decodesub($leftover); $leftover = '';
    }
    # output it
    print $_;
}

# b64decodesub -- takes some characters in the base64 alphabet and
# returns the raw bytes that they represent.
sub b64decodesub
{
    local ($_) = @_[0];

    # translate each char to a value in the range 0 to 63
    eval qq{ tr!$base64alphabet!\0-\77!; };
    # keep 6 bits out of every 8, and pack them together
    $_ = unpack('B*', $_); # look at the bits
    s/(..)(......)/$2/g;   # keep 6 bits of every 8
    s/((........)*)(.*)/$1/; # throw away spare bits (not multiple of 8)
    $_ = pack('B*', $_);   # turn the bits back into bytes
    $_; # return
}
