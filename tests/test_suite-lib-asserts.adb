--                         Copyright (C) 2010, Sogilis                       --

with AdaSpecLib.Asserts;

use AdaSpecLib.Asserts;

package body Test_Suite.Lib.Asserts is

   procedure Add_Tests (Ret : in AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Ret.Add_Test (new Test_Assert);
   end Add_Tests;

   --  Test_Assert  -----------------------------------------------------------

   function  Name (T : in Test_Assert) return String is
      pragma Unreferenced (T);
   begin
      return "AdaSpecLib.Asserts";
   end Name;

   procedure Run (T : in out Test_Assert) is
      package Lib renames AdaSpecLib.Asserts;
   begin
      Lib.Assert (True, "This error shouldn't happen");
      begin
         Lib.Assert (False, "errmsg");
         T.Assert (False, "Assert should raise AdaSpecLib.Asserts.Error");
      exception
         when E : Lib.Error =>
            T.Assert (Exception_Message (E) = "errmsg",
                      "Exception message not OK. Found: '" &
                      Exception_Message (E) & "'");
      end;
      Lib.Equals ("a", "a", "This error shouldn't happen");
      begin
         Lib.Equals ("a", "b", "errmsg");
         T.Assert (False, "Assert should raise AdaSpecLib.Asserts.Error");
      exception
         when Lib.Error =>
            T.Assert (True, "");
      end;
      begin
         Lib.Equals ("a", "b");
         T.Assert (False, "Assert should raise AdaSpecLib.Asserts.Error");
      exception
         when Lib.Error =>
            T.Assert (True, "");
      end;
   end Run;

end Test_Suite.Lib.Asserts;

