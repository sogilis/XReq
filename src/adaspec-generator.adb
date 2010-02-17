--                         Copyright (C) 2010, Sogilis                       --

with AdaSpec.Generator.Ada;

package body AdaSpec.Generator is

   procedure Generate (Job : in Job_Type;
                       Env : in Job_Environment)
   is
   begin
      Ada.Generate (Job, Env);
   end Generate;

end AdaSpec.Generator;
