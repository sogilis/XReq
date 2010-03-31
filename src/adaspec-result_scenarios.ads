--                         Copyright (C) 2010, Sogilis                       --

with Ada.Containers.Vectors;
with AdaSpecLib;
with AdaSpecLib.Generic_Scenarios;
with Util.IO;
with AdaSpec.Result_Steps;
with AdaSpec.Scenarios;
with AdaSpec.Step_Definitions;

use AdaSpecLib;
use Util.IO;
use AdaSpec.Result_Steps;
use AdaSpec.Scenarios;
use AdaSpec.Step_Definitions;

package AdaSpec.Result_Scenarios is

   ----------------------------
   --  Result_Scenario_Type  --
   ----------------------------

   --  Contain a list of procedure names matching the step definitions along
   --  with their parameters.

   package Scenarios_Package is new AdaSpecLib.Generic_Scenarios
      (Result_Step_Type, AdaSpec.Result_Steps.Equals);

   type Result_Scenario_Type is new
      Scenarios_Package.Scenario_Type with private;

   function  To_String        (Res      : in     Result_Scenario_Type;
                               Indent   : in     String := "")
                                          return String;
   procedure Append           (Res      : in out Result_Scenario_Type;
                               Step     : in     Result_Step_Type);
   procedure Process_Scenario (Res      : out    Result_Scenario_Type;
                               Scenario : in     Scenario_Type;
                               Steps    : in     Step_Definitions_Type;
                               Log      : in     Logger_Ptr;
                               Errors   : out    Boolean;
                               Step_Matching : in Boolean := False);

   function  New_Scenario (Name     : in     String;
                           Position : in     Position_Type := Null_Position;
                           Outline  : in     Boolean := False;
                           Tags     : in     String_Vector :=
                                             String_Vectors.Empty_Vector)
                                      return Result_Scenario_Type;

   function  Outline_First (S : in Result_Scenario_Type) return Natural;
   function  Outline_Last  (S : in Result_Scenario_Type) return Integer;
   function  Outline_Count (S : in Result_Scenario_Type) return Natural;
   function  Outline_Step_First   (S       : in Result_Scenario_Type;
                                   Outline : in Natural)  return Natural;
   function  Outline_Step_Last    (S       : in Result_Scenario_Type;
                                   Outline : in Natural)  return Integer;
   function  Outline_Step_Count   (S       : in Result_Scenario_Type;
                                   Outline : in Natural)  return Natural;
   function  Outline_Step_Element (S       : in Result_Scenario_Type;
                                   Outline : in Natural;
                                   Step    : in Natural)
                                   return Result_Step_Type;

private

   package Result_Steps is new Ada.Containers.Vectors
      (Natural, Result_Step_Type, AdaSpec.Result_Steps.Equals);

   package Result_Steps_Vectors2 is
      new Ada.Containers.Vectors
         (Natural, Result_Steps.Vector, Result_Steps."=");

   type Result_Scenario_Type is new Scenarios_Package.Scenario_Type with
      record
         Scenarios : Result_Steps_Vectors2.Vector;
      end record;

end AdaSpec.Result_Scenarios;
