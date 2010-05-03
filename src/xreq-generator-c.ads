--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with Util.Strings;
with Util.Strings.Pool;
with XReqLib;
with XReq.Result;

use Ada.Strings.Unbounded;
use Util.Strings;
use Util.Strings.Pool;
use XReqLib;
use XReq.Result;

package XReq.Generator.C is

   type C_Generator_Type is new Generator_Type with private;
   type C_Generator_Ptr is access all C_Generator_Type'Class;

   procedure Make      (Gen : out    C_Generator_Type;
                        Job : in     Job_Type;
                        Env : in     Job_Environment);

   procedure Generate  (Gen : in out C_Generator_Type;
                        Log : in     Logger_Ptr);

   function  Full_Name (Gen : in     C_Generator_Type) return String;

   procedure Generate_Suite (Gens : in Generator_Vectors.Vector;
                             Name : in String;
                             Env  : in Job_Environment;
                             Log  : in Logger_Ptr;
                             Make : in Boolean := False);

private

   type C_Generator_Type is new Generator_Type with
      record
         Feature      : Result_Feature_Type;
         H_File       : Unbounded_String;
         C_File       : Unbounded_String;
         H            : Buffer_Type;
         C            : Buffer_Type;
         Pool         : String_Pool;
         Header_Name  : Unbounded_String;
         Fn_Backgnd   : Unbounded_String;
         Fn_Steps     : String_Vector;
         Headers      : String_Sets.Set;
         C_Steps      : String_Sets.Set;
      end record;


end XReq.Generator.C;
