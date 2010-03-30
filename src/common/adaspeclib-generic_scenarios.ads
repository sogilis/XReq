--                         Copyright (C) 2010, Sogilis                       --

generic

   type Step_Type is private;
   with function "=" (Left, Right : in Step_Type) return Boolean;

package AdaSpecLib.Generic_Scenarios is

   ---------------------
   --  Scenario_Type  --
   ---------------------

   type Scenario_Type is tagged private;
   type Scenario_Ptr is access all Scenario_Type'Class;
   Null_Scenario : constant Scenario_Type;

   function New_Scenario (Outline  : in Boolean;
                          Name     : in String;
                          Position : in Position_Type) return Scenario_Type;

private

   Null_Scenario : constant Scenario_Type := (others => <>);

   type Scenario_Record (Outline : Boolean := False) is
      record
         Name          : Unbounded_String;
         Pos           : Position_Type;
         Tags          : String_Vector;
         Stanzas       : Step_Vectors.Vector;
         case Outline is
            when True =>
               Table   : AdaSpecLib.String_Tables.Table;
            when False =>
               null;
         end case;
      end record;

   type Scenario_Type is tagged
      record
         D : Scenario_Record;
      end record;

end AdaSpecLib.Generic_Scenarios;
