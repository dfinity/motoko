#!/usr/bin/env perl

use strict;
use warnings;

my $depth = 0;
my $events = 0;

my @stack = ();
my $last_count = 0;

my %immediate;
my %under_call;
my %calls;
my %flamegraph;

if (@ARGV < 1) {
  printf STDERR "Usage $0 [raw|callgrind|flamegraph] [input.wasm]\n";
  exit 1;
}

my $mode = $ARGV[0];

unless ($mode eq "callgrind" || $mode eq "flamegraph" || $mode eq "raw") {
  printf STDERR "Unsupported mode \"$mode\".\n";
  printf STDERR "Supported modes: raw callgrind flamegraph\n";
  exit 1;
}

my %names;
if (@ARGV == 2) {
  my $wasmfile = $ARGV[1];
  my $dump = `wasm-objdump -x -j name "$wasmfile"`;
  while ($dump =~ /^ - func\[(\d+)\] <(.*)>$/mg) {
    $names{$1} = $2;
  }
}

sub fun_name ($) {
  my $fn = shift;
  if (exists $names{$fn} ) {
    return sprintf "%s (%u)", $names{$fn}, $fn;
  } else {
    return sprintf "func%u", $fn;
  }
}

while (<STDIN>) {
  while (/<pRf([A-P]{8})([A-P]{16})>/sg){
    $events++;

    my $fn = 0;
    for my $c (reverse (split //, $1)) { $fn *= 16; $fn += ord($c) - ord('A') };
    my $count = 0;
    for my $c (reverse (split //, $2)) { $count *= 16; $count += ord($c) - ord('A') };

    my $diff = $count - $last_count;
    $last_count = $count;

    if (scalar @stack > 0) {
      $immediate{$stack[-1]} += $diff;

      for (my $i = 0; $i < scalar @stack - 1; $i++) {
        $under_call{$stack[$i]} ||= {};
        $under_call{$stack[$i]}->{$stack[$i+1]} += $diff;
        $calls{$stack[$i]} ||= {};
        $calls{$stack[$i]}->{$stack[$i+1]}++;
      }
      $flamegraph{join(";", map { $names{$_} || "func$_" }  @stack)} += $diff;
    } else {
      if ($diff > 0) {
        printf STDERR "No stack? Losing %u cycles\n", $diff;
      }
    }

    if ($fn != 2**32-1) {
      $depth++;
      push @stack, $fn;
      printf "func %4d: %8d (%s)\n", $fn, $count, fun_name($fn) if $mode eq "raw";
    } else {
      printf "   return: %8d (%s)\n", $count, fun_name($fn) if $mode eq "raw";
      $depth--;
      pop @stack;
      if ($depth < 0) { printf STDERR "Warning: More returns than function calls!\n" }
    }
  }
}
printf STDERR "$0: Loaded %u events from %u functions\n", $events, scalar (keys %immediate);

unless ($depth == 0) {
  printf STDERR "Warning: not back at nesting depths 0, but at depth $depth\n";
}

if ($ARGV[0] eq "callgrind") {
  printf "events: instructions\n";
  foreach my $fn (sort keys %immediate) {
    printf "fn=%s\n", fun_name $fn;
    printf "%u %u\n", $fn, $immediate{$fn};
    foreach my $fn2 (sort keys %{$under_call{$fn}}) {
      printf "cfn=%s\n", fun_name $fn2;
      printf "calls=%u %u\n", $calls{$fn}->{$fn2}, $fn2;
      printf "%u %u\n", $fn, $under_call{$fn}->{$fn2};
    }

    printf "\n";
  }
} elsif ($ARGV[0] eq "flamegraph") {
  foreach my $key (sort keys %flamegraph) {
    printf "%s %u\n", $key, $flamegraph{$key};
  }
} else {
  printf STDERR "Unsupported mode \"$ARGV[0]\". Use callgrind or flamegraph\n";
  exit 1;
}
