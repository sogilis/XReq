--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with Util.IO;
with XReqLib;
with XReq.Scenarios;
with XReqLib.Generic_Features;

use Ada.Strings.Unbounded;
use Util.IO;
use XReqLib;
use XReq.Scenarios;

package XReq.Features is

   Parse_Error : exception;

   -------------------
   -- Feature_Type  --
   -------------------

   package Features_Pkg is new XReqLib.Generic_Features
      (Scenario_Type, XReq.Scenarios.Equals);

   subtype Feature_Type is Features_Pkg.Feature_Type;
   subtype Feature_Ptr  is Features_Pkg.Feature_Ptr;

   procedure Free (F : in out Feature_Ptr)  renames Features_Pkg.Free;
   procedure Make (F :    out Feature_Type;
                   Name : in  String := "") renames Features_Pkg.Make;

   Unparsed_Feature : exception renames Features_Pkg.Unparsed_Feature;
   Null_Feature     : constant Feature_Type := Features_Pkg.Null_Feature;

   ------------------------
   -- Feature_File_Type  --
   ------------------------

   type Feature_File_Type is new Feature_Type with private;
   type Feature_File_Ptr  is access all Feature_File_Type'Class;

   procedure Make      (F         : out    Feature_File_Type;
                        File_Name : in     String);
   function  Create    (File_Name : in     String) return Feature_File_Type;
   function  File_Name (F         : in     Feature_File_Type) return String;
   procedure Parse     (F         : in out Feature_File_Type;
                        Log       : in     Logger_Ptr);

   overriding
   function  Parsed    (F         : in     Feature_File_Type) return Boolean;
   overriding
   function  To_String (F         : in     Feature_File_Type) return String;


   Null_Feature_File : constant Feature_File_Type;



private  ----------------------------------------------------------------------

   type Feature_File_Type is new Feature_Type with
      record
         Parsed      : Boolean := False;
         File_Name   : Unbounded_String;
      end record;

   Null_Feature_File : constant Feature_File_Type :=
      Feature_File_Type'(Null_Feature with others => <>);

end XReq.Features;
