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

with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with GNAT.Regpat;
with Util.IO;
with Util.Strings;
with XReqLib;
with XReq.Lang;
with XReq.Steps;

use Ada.Strings.Unbounded;
use Util.IO;
use Util.Strings;
use XReqLib;
use XReq.Lang;
use XReq.Steps;

package XReq.Step_Definitions is

   --  Result values for the step matching algorithm  -------------------------

   Ambiguous_Match : exception;

   type Match_Location is  --  GCOV_IGNORE
      record
         First : Natural;
         Last  : Natural;
      end record;

   package Match_Vectors is
      new Ada.Containers.Vectors (Natural, Match_Location, "=");

   type Step_Match_Type is
      record
         Match     : Boolean := False;
         Proc_Name : Unbounded_String;
         Matches   : Match_Vectors.Vector;
         Position  : Position_Type;
      end record;

   -------------------------------  abstract type  ----------------------------
   --  Step_File_Type  --
   ----------------------

   type Step_File_Type is abstract tagged private;
   type Step_File_Ptr  is access all Step_File_Type'Class;

   Unparsed_Step : exception;

   function  File_Name (S       : in     Step_File_Type) return String;
   function  Parsed    (S       : in     Step_File_Type) return Boolean;
   procedure Parse     (S       : in out Step_File_Type;
                        Logger  : in     Logger_Ptr) is abstract;

   function  Contains  (S       : in  Step_File_Type;
                        Stanza  : in  Step_Type) return Boolean;

   function  Find      (S       : in  Step_File_Type;
                        Stanza  : in  Step_Type) return String;
   function  Find      (S       : in  Step_File_Type;
                        Stanza  : in  Step_Type)
                        return Step_Match_Type;
   procedure Find      (S       : in  Step_File_Type;
                        Stanza  : in  Step_Type;
                        Proc    : out Unbounded_String;
                        Matches : out Match_Vectors.Vector;
                        Found   : out Boolean);
   procedure Finalize  (S       : in out Step_File_Type);

   procedure Free      (S : in out Step_File_Ptr);


   ----------------------------------------------------------------------------
   --  Step_Definition_Files_Type  --
   ----------------------------------

   package Step_Definition_Vectors is
      new Ada.Containers.Vectors (Natural, Step_File_Ptr, "=");

   subtype Step_File_List_Type is     --  GCOV_IGNORE
      Step_Definition_Vectors.Vector;   --  GCOV_IGNORE
   type Step_File_List_Ptr is access all Step_File_List_Type;

   function  Load      (Directory  : in     String;
                        Language   : in     Language_Type)
                                     return Step_File_List_Type;
   --  IMPORTANT: deallocate Steps_Type

   procedure Load      (Steps      : in out Step_File_List_Type;
                        Logger     : in     Logger_Ptr;
                        Directory  : in     String;
                        Language   : in     Language_Type;
                        Fill_Steps : in     Boolean := False);
   --  IMPORTANT: deallocate Steps_Type

   procedure Add_Steps (Steps      : in out Step_File_List_Type;
                        New_Steps  : in     String_Set;
                        Step_Pkg   : in     String;
                        Directory  : in     String;
                        Language   : in     Language_Type;
                        Logger     : in     Logger_Ptr);

   function  Contains  (Steps      : in  Step_File_List_Type;
                        Stanza     : in  Step_Type) return Boolean;
   function  Find      (Steps      : in  Step_File_List_Type;
                        Stanza     : in  Step_Type) return String;
   function  Find      (Steps      : in  Step_File_List_Type;
                        Stanza     : in  Step_Type) return Step_Match_Type;
   procedure Find      (Steps      : in  Step_File_List_Type;
                        Stanza     : in  Step_Type;
                        Proc       : out Unbounded_String;
                        Matches    : out Match_Vectors.Vector;
                        Found      : out Boolean);

   procedure Free      (Steps      : in out Step_File_List_Type);

private  ----------------------------------------------------------------------

   type Pattern_Matcher_Ptr is                  --  GCOV_IGNORE
      access all GNAT.Regpat.Pattern_Matcher;   --  GCOV_IGNORE

   procedure Free is new Ada.Unchecked_Deallocation
      (GNAT.Regpat.Pattern_Matcher, Pattern_Matcher_Ptr);

   type Step_Definition_Type is
      record
         Prefix    : Step_Kind;
         Pattern_R : Pattern_Matcher_Ptr;
         Pattern_S : Unbounded_String;
         Proc_Name : Unbounded_String;
         Position  : Position_Type;
      end record;

   package Step_Container is new
      Ada.Containers.Vectors (Natural, Step_Definition_Type);

   procedure Finalize (Steps : in out Step_Container.Vector);

   type Step_File_Type is abstract tagged
      record
         Parsed     : Boolean := False;
         File_Name  : Unbounded_String;
         Steps      : Step_Container.Vector;
      end record;

end XReq.Step_Definitions;
