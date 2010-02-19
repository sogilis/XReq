--                         Copyright (C) 2010, Sogilis                       --

with Ada.Containers.Vectors;
with AdaSpec.Job;

use AdaSpec.Job;

package AdaSpec.Generator is

   type Generator_Type is interface;
   type Generator_Ptr is access all Generator_Type'Class;

   procedure Generate (Job : in  Job_Type;
                       Env : in  Job_Environment;
                       Gen : out Generator_Ptr);

   procedure Make     (Gen : out    Generator_Type;
                       Job : in     Job_Type;
                       Env : in     Job_Environment) is abstract;

   procedure Generate (Gen : in out Generator_Type) is abstract;

   package Generator_Vectors is new
      Ada.Containers.Vectors (Natural, Generator_Ptr, "=");

end AdaSpec.Generator;
