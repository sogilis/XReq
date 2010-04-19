--                         Copyright (C) 2010, Sogilis                       --

with XReqLib.Format;
with XReqLib.Format.Text;

use XReqLib.Format;
use XReqLib.Format.Text;

package body Test_Suite.Lib.Format.Text is

   procedure Add_Tests (Ret : in AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Ret.Add_Test (new Test_1);
   end Add_Tests;

   --  Test_1  ----------------------------------------------------------------

   function  Name (T : in Test_1) return String is
      pragma Unreferenced (T);
   begin
      return "XReqLib.Format.Text";
   end Name;

   procedure Run (T : in out Test_1) is
      pragma Unreferenced (T);
      Format : Format_Ptr := Format_Ptr (New_Text_Format);
   begin

      Free (Format);

   end Run;

end Test_Suite.Lib.Format.Text;

