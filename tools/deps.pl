#!/usr/bin/perl

use strict;
use warnings;

use File::Find;

my $root;
my $reverse = 0;
my $maxdepth = -1;

for(;;){
  my $a = shift @ARGV;
  if($a eq "-r" or $a eq "--reverse") {
    $reverse = 1;
  } elsif($a eq "-d" or $a eq "--max-depth") {
    $maxdepth = shift @ARGV;
  } elsif($a eq "-h" or $a eq "--help") {
    print "deps.pl  -  compute dependencies for Ada\n";
    print "\n";
    print "deps.pl -h\n";
    print "deps.pl [-r] [-d MAXDEPTH] ROOT DIR...\n";
    print "\n";
    print "Display a tree of dependencies of ROOT, looking at Ada files in\n";
    print "DIR. It can also display reverse dependancies with -r\n";
    exit;
  } else {
    $root = $a;
    last;
  }
}

my %all;
my %reverse_dep;
my @roots;

sub wanted {
  /\.ad[bs]$/ or return;
  open FILE, "<", $_;
  my @deps;
  my $type = 0;
  my $name = "";
  my $line;
  foreach $line (<FILE>) {
    if ($line =~ m/with ([A-Za-z0-9_\.]+);/) {
      push @deps, "spec $1";
    } elsif ($line =~ m/^package ([A-Za-z0-9_\.]+) is/) {
      $type = "spec";
      $name = $1;
      last;
    } elsif ($line =~ m/^package body ([A-Za-z0-9_\.]+) is/) {
      $type = "body";
      $name = $1;
      last;
    } elsif ($line =~ m/^procedure ([A-Za-z0-9_\.]+) is/) {
      $type = "proc";
      $name = $1;
      last;
    }
  }
  return unless $name;
  if ($type eq "spec" and $name =~ m/^(.*)\.[^\.]*/) {
    unshift @deps, "$type $1";
  }
  if ($type eq "body"){
    push @deps, "spec $name";
  }
  my $pkg = "$type $name";
  $all{$pkg} = \@deps;
  $reverse_dep{$pkg} = [];
  if ($pkg =~ m/$root/i){
    push @roots, $pkg;
  }
  close FILE;
}

find (\&wanted, @ARGV);

# Reverse dependancies and find roots

foreach my $pkg (keys %all) {
  my @deps = @{$all{$pkg}};
  # print "$pkg depends on @deps\n";
  foreach my $dep (@deps) {
    push @{$reverse_dep{$dep}}, $pkg;
  }
}

# Display reverse dependancies

# print "\n\n";
# foreach my $pkg (keys %all) {
#   my @rdeps = @{$reverse_dep{$pkg}};
#   print "$pkg influences @rdeps\n";
# }

print "Roots: @roots\n";

sub print_deps {
  my ($pkg, $spc, $depth) =  @_;
  return unless $all{$pkg};
  return if $depth == 0;
  my @deps;
  if($reverse){
    @deps = @{$reverse_dep{$pkg}};
  } else {
    @deps = @{$all{$pkg}};
  }
  print "$spc$pkg\n";
  foreach my $dep (@deps) {
    print_deps ($dep, "$spc  ", $depth - 1);
  }
}

foreach my $root (@roots) {
  print_deps ($root, "", $maxdepth);
}


