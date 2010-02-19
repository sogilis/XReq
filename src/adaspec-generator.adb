--                         Copyright (C) 2010, Sogilis                       --

with AdaSpec.Generator.Ada;

package body AdaSpec.Generator is

   procedure Generate (Job : in  Job_Type;
                       Env : in  Job_Environment;
                       Gen : out Generator_Ptr)
   is
      Ada_Gen : constant Ada.Ada_Generator_Ptr := new Ada.Ada_Generator_Type;
   begin
      Ada_Gen.Make (Job, Env);
      Ada_Gen.Generate;
      Gen := Generator_Ptr (Ada_Gen);
   end Generate;

end AdaSpec.Generator;
