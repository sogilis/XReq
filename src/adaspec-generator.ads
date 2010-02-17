--                         Copyright (C) 2010, Sogilis                       --

with AdaSpec.Job;

use AdaSpec.Job;

package AdaSpec.Generator is

   procedure Generate (Job : in Job_Type;
                       Env : in Job_Environment);

end AdaSpec.Generator;
