#!/usr/bin/env perl
#
# See README.md for the expected format
#

use strict;
use warnings;

my $block = '';
my $mod = shift @ARGV;

OUTER: while (<>) {
  if (m,^(\s*)/\*\*$,) {
    my $indent = $1;
    my $start = $.;
    die "Please put an empty line before new block in $.\n" if $block;
    $block = '';
    while (<>) {
      if    (m,^$indent ?\*/$,) { $block .= "\n"; next OUTER; }
      elsif (m,^$indent(.*)$,) { $block .= "$1\n" }
      elsif (m,^\s*$,) { $block .= "\n" }
      else { die "Wrong indentation in doc comment at line $..\n" };
    }
    die "Doc comment in line $start not closed.\n";
  } elsif (m,^(?:\s*)public let\s*([a-zA-Z0-9]*)\s*:\s*([^=]*?)\s*=,) {
    my $name = $1;
    my $type = $2;
    print <<__END__;
[#${mod}_$name]
== `$name`
$block

[listing]
$name : $type

__END__
    $block = ''
  } elsif (m,^(\s*)public type\s*([a-zA-Z0-9]*)(.*)$,) {
    my $indent = $1;
    my $name = $2;
    my $def = $3;
    while (<>) {
      if (m,^\s*$,) { last }
      elsif (m,^$indent(.*)$,) { $def .= "$1\n" }
      else { die "Wrong indentation in type def at line $..\n" };
    }
    print <<__END__;
[#${mod}_$name]
== `$name` (type)
$block

....
type $name$def
....

__END__
    $block = '';
  } elsif ($block and m,^\s*$,) {
    print "$block\n";
    $block = ''
  } elsif ($block) {
    die "Please put an empty line after the block at line $.\n" if $block;
  }
}

