--                         Copyright (C) 2010, Sogilis                       --

with Ada.Unchecked_Deallocation;
with Ada.Containers.Vectors;
with AdaSpec.Job;

use AdaSpec.Job;

package AdaSpec.Generator is

   type Generator_Type is interface;
   type Generator_Ptr is access all Generator_Type'Class;

   procedure Generate  (Job : in  Job_Type;
                        Env : in  Job_Environment);

   procedure Generate  (Job : in  Job_Type;
                        Env : in  Job_Environment;
                        Gen : out Generator_Ptr);

   procedure Make      (Gen : out Generator_Type;
                        Job : in  Job_Type;
                        Env : in  Job_Environment) is abstract;

   function  Full_Name (Gen : in  Generator_Type) return String is abstract;

   procedure Generate (Gen : in out Generator_Type) is abstract;

   procedure Free is new Ada.Unchecked_Deallocation
      (Generator_Type'Class, Generator_Ptr);

   package Generator_Vectors is new
      Ada.Containers.Vectors (Natural, Generator_Ptr, "=");

   procedure Generate_Suite (Gens : in Generator_Vectors.Vector;
                             Name : in String;
                             Env  : in Job_Environment);

end AdaSpec.Generator;
