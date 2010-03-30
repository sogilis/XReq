--                         Copyright (C) 2010, Sogilis                       --

with Ada.Containers.Vectors;
with AdaSpecLib;
with AdaSpecLib.Generic_Scenarios;
with Util.IO;
with AdaSpec.Result_Steps;
with AdaSpec.Steps;
with AdaSpec.Scenarios;
with AdaSpec.Step_Definitions;

use AdaSpecLib;
use Util.IO;
use AdaSpec.Result_Steps;
use AdaSpec.Steps;
use AdaSpec.Scenarios;
use AdaSpec.Step_Definitions;

package AdaSpec.Result_Scenarios is

   ----------------------------
   --  Result_Scenario_Type  --
   ----------------------------

   --  Contain a list of procedure names matching the step definitions along
   --  with their parameters.

   package Scenarios_Package is new AdaSpecLib.Generic_Scenarios
      (Result_Step_Type, AdaSpec.Steps.Equals);

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

private

   package Result_Steps is new Ada.Containers.Vectors
      (Natural, Result_Step_Type, AdaSpec.Steps.Equals);

   package Result_Steps_Vectors2 is
      new Ada.Containers.Vectors
         (Natural, Result_Steps.Vector, Result_Steps."=");

   type Result_Scenario_Type is new Scenarios_Package.Scenario_Type with
      record
         Scenarios : Result_Steps_Vectors2.Vector;
      end record;

end AdaSpec.Result_Scenarios;
