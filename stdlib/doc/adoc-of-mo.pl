#!/usr/bin/env perl
#
# This pipe replaces:
#
#     /**
#     Foobar
#     */
#
# with
#
#   Foobar
#
# with extra indentation removed.
#
# The next line must be blank or begin with
#
#   public let something : type = …
#
# In the latter case it prints
#
#   == `something`
#
#   Foobar
#
#   [listing]
#   foobar : type
#
# TODO: Automatically add anchor, index entries

use strict;
use warnings;

my $block = '';

OUTER: while (<>) {
  if (m,^(\s*)/\*\*$,) {
    my $indent = $1;
    my $start = $.;
    die "Please put an empty line before new block in $.\n" if $block;
    $block = '';
    while (<>) {
      if    (m,^$indent ?\*/$,) { $block .= "\n"; next OUTER; }
      elsif (m,^$indent(.*)$,) { $block .= "$1\n" }
      elsif (m,^\s*$,) { print "\n" }
      else { die "Wrong indentation in doc comment at line $..\n" };
    }
    die "Doc comment in line $start not closed.\n";
  } elsif (m,^(?:\s*)public let\s*([a-zA-Z0-9]*)\s*:\s*([^=]*?)\s*=,) {
    my $name = $1;
    my $type = $2;
    print <<__END__;
== `$1`
$block

[listing]
$name : $type

__END__
    $block = ''
  } elsif ($block and m,^\s*$,) {
    print "$block\n";
    $block = ''
  } elsif ($block) {
    die "Please put an empty line after the block at line $.\n" if $block;
  }
}

