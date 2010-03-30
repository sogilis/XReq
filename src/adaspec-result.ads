--                         Copyright (C) 2010, Sogilis                       --

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Util.IO;
with AdaSpecLib;
with AdaSpecLib.Generic_Scenarios;
with AdaSpecLib.Generic_Features;
with AdaSpecLib.String_Tables;
with AdaSpec.Features;
with AdaSpec.Step_Definitions;
with AdaSpec.Scenarios;
with AdaSpec.Steps;

use Ada.Strings.Unbounded;
use Util.IO;
use AdaSpecLib;
use AdaSpec.Features;
use AdaSpec.Step_Definitions;
use AdaSpec.Scenarios;
use AdaSpec.Steps;

with AdaSpec.Result_Steps;
with AdaSpec.Result_Scenarios;
with AdaSpec.Result_Features;

package AdaSpec.Result is

   subtype Result_Step_Type     is Result_Steps.Result_Step_Type;
   subtype Result_Scenario_Type is Result_Scenarios.Result_Scenario_Type;
   subtype Result_Feature_Type  is Result_Features.Result_Feature_Type;

end AdaSpec.Result;
