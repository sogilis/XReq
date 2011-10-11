#!/usr/bin/perl


$pkgname = "StdIn";
$ADB = 0;
$ADS = 0;

while(1) {
  if ($ARGV[0] eq "-h") {
    print "template.pl [-ads[=FILE]] [-adb[=FILE]] [PACKAGE_NAME [SRC_FILE|-]]\n";
    exit;
  } elsif ($ARGV[0] eq "-ads") {
    $ADS = *STDOUT
  } elsif ($ARGV[0] eq "-adb") {
    $ADB = *STDOUT
  } elsif ($ARGV[0] =~ m/^-ads=(.*)$/) {
    print "Write: $1\n";
    open $ADS, ">$1"
  } elsif ($ARGV[0] =~ m/^-adb=(.*)$/) {
    print "Write: $1\n";
    open $ADB, ">$1"
  } else {
    last;
  }
  shift;
}

if ($ARGV[0]) {
  $pkgname = $ARGV[0];
}

if ($ARGV[1] eq "-" or not $ARGV[1]) {
  $INFO = *STDIN;
} else {
  open $INFO, '<', $ARGV[1];
}


%variables = ();
@variable_order = ();
$current_var = "";

while ($line = <$INFO>) {
  if ($line =~ /^<\?(garbage|var name="([^"]*)")\?> *\n?$/) {
    if ($1 eq "garbage") {
      $current_var = "";
    } else {
      $current_var = $2;
    }
  } elsif ($current_var) {
    if(not $variables{$current_var}) {
      push @variable_order, $current_var;
    };
    $variables{$current_var} .= $line;
  }
}

close $INFO;

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

foreach my $var (@variable_order) {
  my $content = $variables{$var};
  my $proto = "\n";
  $var =~ s/[^A-Za-z0-9_]/_/g;
  $proto   .= "   procedure $var\n";
  $proto   .= "        (File : in out File_Type";
  my %seen = ();
  while ($content =~ m/<\?placeholder name="([^"]*)"\?>/g) {
    if (not $seen{$1}) {
      $proto .= ";\n";
      $proto .= "         Param_$1 : in String";
    }
    $seen{$1} = 1;
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


print $ADS $ads if $ADS;
print $ADB $adb if $ADB;

close $ADS if $ADS;
close $ADB if $ADB;
