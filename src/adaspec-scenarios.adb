--                         Copyright (C) 2010, Sogilis                       --

package body AdaSpec.Scenarios is

   --------------------------
   --  Scenario  --  Make  --
   --------------------------

   procedure Make   (Scenario : out    Scenario_Type;
                     Name     : in     String := "")
   is
      S : Scenario_Type := Null_Scenario;
   begin
      S.Name   := To_Unbounded_String (Name);
      Scenario := S;
   end Make;

   ----------------------------
   --  Scenario  --  Append  --
   ----------------------------

   procedure Append (Scenario : in out Scenario_Type;
                     Stanza   : in     Step_Type)
   is
      use Step_Vectors;
   begin
      Append (Scenario.Stanzas, Stanza);
   end Append;

end AdaSpec.Scenarios;