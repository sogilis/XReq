--                         Copyright (C) 2010, Sogilis                       --

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

use Ada.Strings.Unbounded;

package AdaSpec.Result is

   ------------------------
   --  Result_Step_Type  --
   ------------------------

   --  A procedure name of a step definition and its arguments

   type Result_Step_Type is
      record
         Procedure_Name : Unbounded_String;
      end record;
      --  TODO: I can't make this a private type, or else I get the error
      --  "premature use of private type" for the Result_Steps vector package

   package Result_Steps is
      new Ada.Containers.Vectors (Natural, Result_Step_Type, "=");

   procedure Make (S              : out Result_Step_Type;
                   Procedure_Name : in  String);

   function Procedure_Name (S : in Result_Step_Type) return String;

   ----------------------------
   --  Result_Scenario_Type  --
   ----------------------------

   --  Contain a list of procedure names matching the step definitions along
   --  with their parameters.

   type Result_Scenario_Type is
      record
         Steps : Result_Steps.Vector;
      end record;

   package Result_Scenarios is
      new Ada.Containers.Vectors (Natural, Result_Scenario_Type, "=");

   ---------------------------
   --  Result_Feature_Type  --
   ---------------------------

   --  The Result_Feature_Type describes the result of the combinaison of the
   --  steps in the feature file and the step definitions. It contains the
   --  procedures of the step definitions to call in correct order and their
   --  parameters.

   type Result_Feature_Type is
      record
         Background : Result_Scenario_Type;
         Scenarios  : Result_Scenarios.Vector;
      end record;

end AdaSpec.Result;
