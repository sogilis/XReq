--                         Copyright (C) 2010, Sogilis                       --

with XReqLib.Util;

use XReqLib.Util;

package body Test_Suite.Lib.Util is

   procedure Add_Tests (Ret : in AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Ret.Add_Test (new Test_1);
   end Add_Tests;

   --  Test_1  ----------------------------------------------------------------

   function  Name (T : in Test_1) return String is
      pragma Unreferenced (T);
   begin
      return "XReqLib.Util";
   end Name;

   procedure Run (T : in out Test_1) is
      Ex : exception;
   begin
      raise Ex;
   exception
      when E : Ex =>
         Put_Exception_Information (E);
   end Run;

end Test_Suite.Lib.Util;

