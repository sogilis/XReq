-------------------------------------------------------------------------------
--  XReq  --  Behaviour Driven Developpement tool for compiled languages     --
--  Copyright (c) 2010, SOGILIS <http://sogilis.com>                         --
--                                                                           --
--  This program is free software: you can redistribute it and/or modify     --
--  it under the terms of the GNU Affero General Public License as           --
--  published by the Free Software Foundation, either version 3 of the       --
--  License, or (at your option) any later version.                          --
--                                                                           --
--  This program is distributed in the hope that it will be useful,          --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of           --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            --
--  GNU Affero General Public License for more details.                      --
--                                                                           --
--  You should have received a copy of the GNU Affero General Public License --
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.    --
--                                                                           --
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded;
with Util.IO;
with XReqLib;
with XReq.Scenarios;
with XReqLib.Generic_Features;
with XReq.Language;

use Ada.Strings.Unbounded;
use Util.IO;
use XReqLib;
use XReq.Scenarios;
use XReq.Language;

package XReq.Features is

   Parse_Error : exception;

   --------------------
   --  Feature_Type  --
   --------------------

   package Features_Pkg is new XReqLib.Generic_Features
      (Scenario_Type, XReq.Scenarios.Equals);

   subtype Feature_Type is Features_Pkg.Feature_Type;
   subtype Feature_Ptr  is Features_Pkg.Feature_Ptr;

   procedure Free (F : in out Feature_Ptr)  renames Features_Pkg.Free;
   procedure Make (F :    out Feature_Type;
                   Name : in  String := "") renames Features_Pkg.Make;

   Unparsed_Feature : exception renames Features_Pkg.Unparsed_Feature;
   Null_Feature     : constant Feature_Type := Features_Pkg.Null_Feature;

   ----------------------------
   --  Generic_Feature_Type  --
   ----------------------------

   type Generic_Feature_Type is new Feature_Type with private;
   type Generic_Feature_Ptr  is access all Generic_Feature_Type'Class;

   function  Language  (F : in Generic_Feature_Type) return Language_Type;

   -------------------------
   --  Feature_File_Type  --
   -------------------------

   type Feature_File_Type is new Generic_Feature_Type with private;
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

   type Generic_Feature_Type is new Feature_Type with
      record
         Lang : Language_Handle;
      end record;

   type Feature_File_Type is new Generic_Feature_Type with
      record
         Parsed      : Boolean := False;
         File_Name   : Unbounded_String;
      end record;

   Null_Feature_File : constant Feature_File_Type :=
      Feature_File_Type'(Null_Feature with others => <>);

end XReq.Features;
