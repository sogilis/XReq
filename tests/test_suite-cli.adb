--                         Copyright (C) 2010, Sogilis                       --

with AdaSpec.CLI;

--  with AUnit.Assertions; use AUnit.Assertions;

package body Test_Suite.CLI is

   function Name (T : in Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test Command Line Interface");
   end Name;

   procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
   begin
      AdaSpec.CLI.Help;
      --  TODO: what can be dont to test that it is ok ?
   end Run_Test;

end Test_Suite.CLI;
