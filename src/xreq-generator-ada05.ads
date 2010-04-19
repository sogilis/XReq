--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with XReqLib;
with Util.Strings;
with Util.Strings.Pool;
with XReq.Result;

use Ada.Strings.Unbounded;
use XReqLib;
use Util.Strings;
use Util.Strings.Pool;
use XReq.Result;

package XReq.Generator.Ada05 is

   type Ada_Generator_Type is new Generator_Type with private;
   type Ada_Generator_Ptr is access all Ada_Generator_Type'Class;

   procedure Make      (Gen : out    Ada_Generator_Type;
                        Job : in     Job_Type;
                        Env : in     Job_Environment);

   procedure Generate  (Gen : in out Ada_Generator_Type;
                        Log : in     Logger_Ptr);

   function  Full_Name (Gen : in     Ada_Generator_Type) return String;

   procedure Generate_Suite (Gens : in Generator_Vectors.Vector;
                             Name : in String;
                             Env  : in Job_Environment;
                             Log  : in  Logger_Ptr;
                             Make : in Boolean := False);

private

   type Ada_Generator_Type is new Generator_Type with
      record
         Feature    : Result_Feature_Type;
         Ads_File   : Unbounded_String;
         Adb_File   : Unbounded_String;
         Adb        : Buffer_Type;
         Ads        : Buffer_Type;
         Pool       : String_Pool;
         Fn_Backgnd : Unbounded_String;
         Id_Pkgname : Unbounded_String;
         With_Pkg   : String_Sets.Set;
         Fn_Steps   : String_Vector;
      end record;


end XReq.Generator.Ada05;
