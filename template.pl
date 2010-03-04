#!/usr/bin/perl

if ($ARGV[0] eq "-h") {
  print "template.pl [FILE [PACKAGE_NAME [ADS [ADB]]]]";
  exit;
}

$pkgname = "StdIn";

if ($ARGV[0] eq "-" or not $ARGV[0]) {
  $INFO = *STDIN;
} else {
  $pkgname = $ARGV[0];
  $pkgname =~ s/\./_/g;
  open INFO, $ARGV[0];
  $INFO = *INFO;
}

if ($ARGV[1]) {
  $pkgname = $ARGV[1];
}

$ads_file = $ARGV[2];
$adb_file = $ARGV[3];

if ($ads_file) {

  if ($ads_file =~ m/\.adb$/) {
    $ads_file =~ s/\.adb$/.ads/;
  } elsif ($ads_file !~ m/\.ads$/) {
    $ads_file =~ s/$/.ads/;
  }

  if ($ads_file and not $adb_file) {
    $adb_file = $ads_file;
    $adb_file =~ s/\.ads$/.adb/;
  }

}

open ADS, ">$ads_file" if $ads_file;
open ADB, ">$adb_file" if $adb_file;

print "Write: $ads_file\n" if $ads_file;
print "Write: $adb_file\n" if $adb_file;

%variables = ();
$current_var = "";

while ($line = <$INFO>) {
  if ($line =~ /^<\?(garbage|var name="([^"]*)")\?> *\n?$/) {
    if ($1 eq "garbage") {
      $current_var = "";
    } else {
      $current_var = $2;
    }
  } elsif ($current_var) {
    $variables{$current_var} .= $line;
  }
}

close INFO;

$pkgname =~ s/[\.\-]/./g;
$pkgname =~ s/[^A-Za-z0-9_\.]/_/g;

$ads = "";
$adb = "";

$ads .= "generic\n";
$ads .= "   type File_Type (<>) is limited private;\n";
$ads .= "   with procedure Put (File : in out File_Type; Item : in String);\n";
$ads .= "package $pkgname is\n\n";
$adb .= "package body $pkgname is\n\n";

$ads .= "   pragma Style_Checks (Off);\n";
$adb .= "   pragma Style_Checks (Off);\n";

while (($var, $content) = each(%variables)) {
  $proto = "\n";
  $var =~ s/[^A-Za-z0-9_]/_/g;
  $proto   .= "   procedure $var\n";
  $proto   .= "        (File : in out File_Type";
  while ($content =~ m/<\?placeholder name="([^"]*)"\?>/g) {
    $proto .= ";\n";
    $proto .= "         Param_$1 : in String";
  }
  $proto   .= ")";
  $ads .= "$proto;\n";
  $adb .= "$proto is\n";
  $adb .= "   begin\n";
  foreach my $line (split("\n", $content)) {
    while ($line =~ /(<\?placeholder name="([^"]*)"\?>)/g) {
      my $all = $`.$1;
      my $chunk = $`;
      my $varname = $2;
      $chunk =~ s/\"/\"\"/g;
      $adb .= "      Put (File, \"$chunk\");\n" if $chunk;
      $adb .= "      Put (File, Param_$varname);\n";
      $line = substr ($line, length($all));
    }
    $line =~ s/\"/\"\"/g;
    $adb .= "      Put (File, \"$line\" & ASCII.LF);\n";
  }
  $adb .= "   end $var;\n";
}

$ads .= "\n   pragma Style_Checks (On);\n";
$adb .= "\n   pragma Style_Checks (On);\n";

$ads .= "\nend $pkgname;\n";
$adb .= "\nend $pkgname;\n";


print ADS $ads if $ads_file;
print $ads unless $ads_file;
print "\n" unless $ads_file or $adb_file;
print $adb unless $adb_file;
print ADB $adb if $adb_file;

close ADS;
close ADB;
