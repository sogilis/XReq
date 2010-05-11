#!/usr/bin/perl -pi

s/((procedure|function)\s*([A-Za-z][A-Za-z0-9_]*)\s*(\(.*?\))?\s*(return\s.*?)?\s*);/$1 is\n   begin\n      null;  --  TODO\n   end $3;\n/msg;

