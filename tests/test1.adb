--                         Copyright (C) 2010, Sogilis                       --

with AUnit.Assertions; use AUnit.Assertions;

package body Test1 is

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test1 HelloWorld");
   end Name;

   procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Assert (True, "True test");
   end Run_Test;

end Test1;
