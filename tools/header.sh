#!/bin/sh

cd "`dirname "$0"`/.."

mark="--                         Copyright (C) 2010, Sogilis                       --"
headerfile=tools/header.ada.txt
oldheaderfile=tools/header.ada.txt

while read file; do
  line=$(grep -Fn -- "$mark" "$file" | head -n 1 | cut -d: -f1)
  if [ -z "$line" ]; then
    line=$(wc -l "$oldheaderfile" | cut -d" " -f1)
    if [ a"$(head -n $line "$file")" != a"$(cat "$oldheaderfile")" ]; then
      line=
    fi
  fi
  if [ -n "$line" ]; then
    echo "Header: $file"
    line=$(($line+1))
    content="$(tail -n +$line "$file")"
    cat "$headerfile" >"$file"
    echo "$content" >>"$file"
  fi
done <<<"$(find . -name "*.ad[bs]" -o -name "*.gpr")"