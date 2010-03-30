--                         Copyright (C) 2010, Sogilis                       --

with AdaSpecLib.Generic_Scenarios;
with AdaSpec.Steps;

use AdaSpec.Steps;

package AdaSpec.Scenarios is new AdaSpecLib.Generic_Scenarios
   (Step_Type, AdaSpec.Steps.Equals);
