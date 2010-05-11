#!/usr/bin/perl

if ($ARGV[0] eq "-" or not $ARGV[0]) {
  $INFO = *STDIN;
} else {
  open INFO, $ARGV[0];
  $INFO = *INFO;
}

while ($line = <$INFO>) {
  @fields = split(/:/, $line);
  if ($fields[0] eq "SF") {
    $file = $fields[1];
    $file =~ s/\s+$//;
    close SF;
    open SF, $fields[1];
    $l = 0;
    $ignore = 0;
  }
  if ($fields[0] eq "DA") {
    ($da_line, $da_count) = split(/,/, $fields[1]);
    if ($da_count == 0) {
      while ($l < $da_line) {
        $src_line = <SF>;
        $l++;
        if ($src_line =~ /GCOV_IGNORE_BEGIN/) {
          $ignore = 1;
        } elsif ($src_line =~ /GCOV_IGNORE_END/) {
          $ignore = 0;
        }
      }
      if($ignore or $src_line =~ /GCOV_IGNORE/) {
        $line = "DA:$da_line,1\n";
        #next;
      }
    }
  }
  print $line;
}

close SF;
close INFO;
