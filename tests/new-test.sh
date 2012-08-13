#!/usr/bin/env bash

prjdir="$(dirname "$0")/.."
testdir="$(dirname "$0")"

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
  test_package="$tested_package.Test_Suite"
  echo "Test package: $test_package"
fi

filename="$(tr A-Z a-z <<<"$test_package" | tr . -)"

echo "File Name: $filename.ad[bs]"

if ! $force && ( [ -e "$filename.adb" ] || [ -e "$filename.ads" ] ); then
  echo "Error: $filename.ad[bs] exists"
  exit 1
fi

cat >"$testdir/$filename.ads" <<EOF
$(cat "$prjdir/tools/header.ada.txt")

with AUnit;
with AUnit.Test_Suites;
with Test_Suite;

package $test_package is

   procedure Add_Tests (Ret : in AUnit.Test_Suites.Access_Test_Suite);

   --  Test type
   type Test_1 is new Test_Suite.Test_Case_Type with null record;

   --  Operation on Test_1
   function  Name (T : in     Test_1) return String;
   procedure Run  (T : in out Test_1);

end $test_package;

EOF


cat >"$testdir/$filename.adb" <<EOF
$(cat "$prjdir/tools/header.ada.txt")

with $tested_package;

use $tested_package;

package body $test_package is

   procedure Add_Tests (Ret : in AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Ret.Add_Test (new Test_1);
   end Add_Tests;

   --  Test_1  ----------------------------------------------------------------

   function  Name (T : in Test_1) return String is
      pragma Unreferenced (T);
   begin
      return "$tested_package";
   end Name;

   procedure Run (T : in out Test_1) is
   begin

      T.Assert (False, "Missing test for $tested_package");

   end Run;

end $test_package;

EOF

echo "Don't forget to modify the test suite"
