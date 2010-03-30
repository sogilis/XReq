--                         Copyright (C) 2010, Sogilis                       --

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Util.Strings;
with AdaSpecLib;
with AdaSpecLib.String_Tables;
with AdaSpec.Steps;

use Ada.Strings.Unbounded;
use Util.Strings;
use AdaSpecLib;
use AdaSpec.Steps;

package AdaSpec.Scenarios is

   package Step_Vectors is
      new Ada.Containers.Vectors (Natural, Step_Type, "=");

   ---------------------
   --  Scenario_Type  --
   ---------------------

   type Scenario_Type (Outline : Boolean := False) is
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
   type Scenario_Ptr is access all Scenario_Type;
   Null_Scenario : constant Scenario_Type := (others => <>);

   procedure Make   (Scenario : out    Scenario_Type;
                     Name     : in     String := "");
   procedure Append (Scenario : in out Scenario_Type;
                     Stanza   : in     Step_Type);

end AdaSpec.Scenarios;