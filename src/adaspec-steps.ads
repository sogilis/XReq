--                         Copyright (C) 2010, Sogilis                       --

with AdaSpec.Generic_Steps;
with AdaSpec.Args;

use AdaSpec.Args;

package AdaSpec.Steps is new AdaSpec.Generic_Steps
  (Argument_Type, "=");
