--                         Copyright (C) 2010, Sogilis                       --

with AdaSpec.Result_Steps;
with AdaSpec.Result_Scenarios;
with AdaSpec.Result_Features;

package AdaSpec.Result is

   subtype Result_Step_Type     is Result_Steps.Result_Step_Type;
   subtype Result_Scenario_Type is Result_Scenarios.Result_Scenario_Type;
   subtype Result_Feature_Type  is Result_Features.Result_Feature_Type;

end AdaSpec.Result;
