--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with AUnit.Assertions;
with Util.Strings.Pool;

use Ada.Strings.Unbounded;
use AUnit.Assertions;
use Util.Strings.Pool;

package body Test_Suite.Strings.Pool is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite)
   is
   begin
      Ret.Add_Test (new Test_1);
   end Add_Tests;

   --  Test_1  ----------------------------------------------------------------

   function  Name (T : in Test_1) return String is
      pragma Unreferenced (T);
   begin
      return ("Util.Strings.Pool");
   end Name;

   procedure Run (T : in out Test_1) is
      pragma Unreferenced (T);
      Pool : String_Pool;
      Str  : Unbounded_String;
   begin

      Get_Unique_String (Pool, "Test1", Str);
      Assert (To_String (Str) = "Test1",
              "Could not insert 'Test1' in pool");

      Get_Unique_String (Pool, "Test1", Str);
      Assert (To_String (Str) /= "Test1",
              "Get_Unique_String do not return a unique string: " &
              To_String (Str));

      Get_Unique_String (Pool, "Test2", Str);
      Assert (To_String (Str) = "Test2",
              "Could not insert 'Test2' in pool");

      Get_Unique_String (Pool, "Test2", Str);
      Assert (To_String (Str) /= "Test2",
              "Get_Unique_String do not return a unique string: " &
              To_String (Str));

   end Run;

end Test_Suite.Strings.Pool;

