--                         Copyright (C) 2010, Sogilis                       --

package body AdaSpec.Scenarios is

   ----------------------------------
   --  Scenario  --  New_Scenario  --
   ----------------------------------

   function  New_Scenario (Name     : in     String;
                           Position : in     Position_Type := Null_Position)
                                      return Scenario_Type
   is
   begin
      return Scenario_Type'(Name   => To_Unbounded_String (Name),
                            Pos    => Position,
                            others => <>);
   end New_Scenario;

   ----------------------------
   --  Scenario  --  Append  --
   ----------------------------

   procedure Step_Append (Scenario : in out Scenario_Type;
                     Stanza   : in     Step_Type)
   is
      use Step_Vectors;
   begin
      Append (Scenario.Steps, Stanza);
   end Step_Append;

   -------------------------------
   --  Scenario  --  Step_First  --
   -------------------------------

   function  Step_First  (Scenario : in     Scenario_Type) return Natural is
      pragma Unreferenced (Scenario);
   begin
      return 0;
   end Step_First;

   -------------------------------
   --  Scenario  --  Step_Last  --
   -------------------------------

   function  Step_Last   (Scenario : in     Scenario_Type) return Integer is
      use Ada.Containers;
      use Step_Vectors;
   begin
      return Integer (Length (Scenario.Steps)) - 1;
   end Step_Last;

end AdaSpec.Scenarios;
