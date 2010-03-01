#!/bin/bash

if [ $# = 0 ]; then
  echo "$0 [-f] TESTED_PKG [TEST_PKG]"
  exit 0
fi

if [ "a$1" = "a-f" ]; then
  force=true
  shift
else
  force=false
fi

tested_package="$1"
test_package="$2"

if [ -z "$test_package" ]; then
  test_package="$(sed -r "s/AdaSpecLib/Test_Suite.Lib/" <<<"$tested_package" | \
                  sed -r "s/(AdaSpec|Util)/Test_Suite/")"
  echo "Test package: $test_package"
fi

filename="$(tr A-Z a-z <<<"$test_package" | tr . -)"

echo "File Name: $filename.ad[bs]"

if ! $force && ( [ -e "$filename.adb" ] || [ -e "$filename.ads" ] ); then
  echo "Error: $filename.ad[bs] exists"
  exit 1
fi

cat >"$filename.ads" <<EOF
--                         Copyright (C) 2010, Sogilis                       --

with AUnit;
with AUnit.Test_Suites;

package $test_package is

   procedure Add_Tests (Ret : in AUnit.Test_Suites.Access_Test_Suite);

   --  Test type
   type Test_1 is new Test_Case_Type with null record;

   --  Operation on Test_1
   function  Name (T : in     Test_1) return String;
   procedure Run  (T : in out Test_1);

end $test_package;

EOF


cat >"$filename.adb" <<EOF
--                         Copyright (C) 2010, Sogilis                       --

with $tested_package;

use $tested_package;

package body $test_package is

   procedure Add_Tests (Ret : in AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Ret.Add_Test (new Test_1);
   end Add_Tests;

   --  Test_1  ----------------------------------------------------------------

   function  Name (T : in Test_1) return String is
   begin
      return "$tested_package";
   end Name;

   procedure Run (T : in out Test_1) is
      pragma Unreferenced (T);
   begin

      T.Assert (False, "Missing test for $tested_package");

   end Run;

end $test_package;

EOF

echo "Don't forget to modify the test suite"
