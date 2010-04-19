--                         Copyright (C) 2010, Sogilis                       --

with XReq.Result_Steps;
with XReq.Result_Scenarios;
with XReq.Result_Features;

package XReq.Result is

   subtype Result_Step_Type     is Result_Steps.Result_Step_Type;
   subtype Result_Scenario_Type is Result_Scenarios.Result_Scenario_Type;
   subtype Result_Feature_Type  is Result_Features.Result_Feature_Type;

end XReq.Result;
