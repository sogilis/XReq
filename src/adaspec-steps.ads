--                         Copyright (C) 2010, Sogilis                       --

with AdaSpecLib.Generic_Steps;
with AdaSpec.Args;

use AdaSpec.Args;

package AdaSpec.Steps is new AdaSpecLib.Generic_Steps
  (Argument_Type, "=");
