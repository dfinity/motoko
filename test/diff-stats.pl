#!/usr/bin/env perl

#
# This script compares two CSV files with performance stats, and produces a
# short textual summary
#

use strict;
use warnings;

die "Usage: $0 file1.csv file2.csv" unless scalar @ARGV == 2;

my %stats;

sub read_file ($$) {
  my $pos = shift;
  my $file = shift;
  open FILE, '<', $file;
  while(<FILE>) {
    m,(.*)/(.*);([0-9]+), or die "Cannot parse line $. of $file";
    $stats{$1} = {} unless exists $stats{$1};
    $stats{$1}{$2} = [0,0] unless exists $stats{$1}{$2};
    $stats{$1}{$2}[$pos] = $3;
  }
}

read_file(0, $ARGV[0]);
read_file(1, $ARGV[1]);

sub comma_and {
  if (scalar @_ == 0) {
    return ""
  } elsif (scalar @_ == 1) {
    return $_[0]
  } elsif (scalar @_ == 2) {
    return (sprintf "%s and %s", @_)
  } else {
    my $head = shift;
    return (sprintf "%s, %s", $head, comma_and(@_));
  }
}

sub bulleted {
  if (scalar @_ == 0) {
    return ""
  } elsif (scalar @_ == 1) {
    return (sprintf " - %s\n", $_[0]);
  } else {
    my $head = shift;
    return (sprintf " - %s\n%s", $head, bulleted(@_));
  }
}

my @report;

for my $group (sort keys %stats) {
  my $gstats = $stats{$group};
  my $total = 0;
  my $any_change = 0;
  my $regressed = 0;
  my $improved = 0;
  my $missing = 0;
  my $new = 0;
  my $geometric_mean = 1;
  my $geometric_mean_denom = 0;

  for my $test (keys %$gstats) {
    $total++;
    my $stat1 = $gstats->{$test}[0];
    my $stat2 = $gstats->{$test}[1];
    if ($stat1 and $stat2) {
      if ($stat1 != $stat2) {
	push @report, sprintf "test `%s` (%s), change %+.5f%% (%d vs. previous %d)", $test, $group, ($stat2/$stat1 - 1) * 100, $stat2, $stat1;
      }
      $geometric_mean *= $stat2/$stat1;
      $geometric_mean_denom++;
      if ($stat1 < $stat2) {
        $regressed++;
      } elsif ($stat1 > $stat2) {
        $improved++;
      }
    } elsif ($stat1 and not $stat2) {
      $missing++;
    } elsif ($stat2 and not $stat1) {
      $new++;
    } else {
      # unlikely
    }
  }

  $geometric_mean = $geometric_mean ** (1/$geometric_mean_denom) if $geometric_mean_denom;

  if ($regressed or $improved or $new or $missing ) {
    my @clauses;
    push @clauses, sprintf "%d tests regressed", $regressed if $regressed;
    push @clauses, sprintf "%d tests improved", $improved if $improved;
    push @clauses, sprintf "%d tests are new", $new if $new;
    push @clauses, sprintf "%d tests are removed", $missing if $missing;
    push @clauses, sprintf "the mean change is %+.5f%%.\n", ($geometric_mean - 1) * 100;
    printf "In terms of %s, %s", $group, comma_and(@clauses);
  } else {
    printf "In terms of %s, no changes are observed in %d tests.\n", $group, $total;
  }
}

if (@report ne "") {
  printf "\n\n### Fine resolution stats\n%s\n", bulleted(@report);
}
