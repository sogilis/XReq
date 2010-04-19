--                         Copyright (C) 2010, Sogilis                       --

with XReqLib.Generic_Scenarios;
with XReq.Steps;

use XReq.Steps;

package XReq.Scenarios is new XReqLib.Generic_Scenarios
   (Step_Type, XReq.Steps.Equals);
