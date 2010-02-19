--                         Copyright (C) 2010, Sogilis                       --

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with AdaSpec.Features;
with AdaSpec.Steps;

use Ada.Strings.Unbounded;
use AdaSpec.Features;
use AdaSpec.Steps;

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

   function  Create (Procedure_Name : in  String) return Result_Step_Type;
   procedure Make   (S              : out Result_Step_Type;
                     Procedure_Name : in  String);

   function Procedure_Name (S      : in Result_Step_Type) return String;
   function To_String      (S      : in Result_Step_Type;
                            Indent : in String := "") return String;

   ----------------------------
   --  Result_Scenario_Type  --
   ----------------------------

   --  Contain a list of procedure names matching the step definitions along
   --  with their parameters.

   type Result_Scenario_Type is
      record
         Name  : Unbounded_String;
         Steps : Result_Steps.Vector;
      end record;

   package Result_Scenarios is
      new Ada.Containers.Vectors (Natural, Result_Scenario_Type, "=");

   function  To_String        (Res      : in     Result_Scenario_Type;
                               Indent   : in     String := "")
                                          return String;
   procedure Append           (Res      : in out Result_Scenario_Type;
                               Step     : in     Result_Step_Type);
   procedure Process_Scenario (Res      : out    Result_Scenario_Type;
                               Scenario : in     Scenario_Type;
                               Steps    : in     Steps_Type;
                               Errors   : out    Boolean);

   ---------------------------
   --  Result_Feature_Type  --
   ---------------------------

   --  The Result_Feature_Type describes the result of the combinaison of the
   --  steps in the feature file and the step definitions. It contains the
   --  procedures of the step definitions to call in correct order and their
   --  parameters.

   type Result_Feature_Type is
      record
         Name       : Unbounded_String;
         Background : Result_Scenario_Type;
         Scenarios  : Result_Scenarios.Vector;
         Fail       : Boolean := False;
      end record;

   function  To_String       (Res      : in     Result_Feature_Type;
                              Indent   : in     String := "")
                                         return String;
   procedure Append          (Res      : in out Result_Feature_Type;
                              Scenario : in     Result_Scenario_Type);
   procedure Process_Feature (Res      : out    Result_Feature_Type;
                              Feature  : in     Feature_Ptr;
                              Steps    : in     Steps_Type);

end AdaSpec.Result;
