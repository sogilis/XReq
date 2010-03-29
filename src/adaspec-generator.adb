--                         Copyright (C) 2010, Sogilis                       --

with AdaSpec.Generator.Ada05;

package body AdaSpec.Generator is

   procedure Generate (Job : in  Job_Type;
                       Env : in  Job_Environment;
                       Log : in  Logger_Ptr;
                       Gen : out Generator_Ptr)
   is
      Ada_Gen : constant Ada05.Ada_Generator_Ptr
              := new Ada05.Ada_Generator_Type;
   begin
      Ada_Gen.Make (Job, Env);
      Ada_Gen.Generate (Log);
      Gen := Generator_Ptr (Ada_Gen);
   end Generate;

   procedure Generate  (Job : in  Job_Type;
                        Env : in  Job_Environment;
                        Log : in  Logger_Ptr)
   is
      Gen : Generator_Ptr;
   begin
      Generate (Job, Env, Log, Gen);
      Free (Gen);
   end Generate;

   procedure Generate_Suite (Gens : in Generator_Vectors.Vector;
                             Name : in String;
                             Env  : in Job_Environment;
                             Log  : in  Logger_Ptr;
                             Make : in Boolean := False)
   is
   begin
      Ada05.Generate_Suite (Gens, Name, Env, Log, Make);
   end Generate_Suite;

end AdaSpec.Generator;
