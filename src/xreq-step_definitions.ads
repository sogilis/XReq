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
with Ada.Containers.Vectors;
with GNAT.Regpat;
with Util.IO;
with XReqLib;
with XReq.Steps.Handles;
with Reffy;

use Ada.Strings.Unbounded;
use Util.IO;
use XReqLib;
use XReq.Steps.Handles;

package XReq.Step_Definitions is

   --  Result values for the step matching algorithm  -------------------------

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

   type Find_Result_Procedure is
     access procedure (Match : Step_Match_Type);

   -------------------------------  abstract type  ----------------------------
   --  Step_File_Type  --
   ----------------------

   type Step_File_Type is abstract new Reffy.Counted_Type with private;
   type Step_File_Ptr  is access all Step_File_Type'Class;

   Unparsed_Step : exception;

   function  File_Name (S       : in     Step_File_Type) return String;
   function  Parsed    (S       : in     Step_File_Type) return Boolean;
   procedure Parse     (S       : in out Step_File_Type;
                        Logger  : in     Logger_Ptr) is abstract;

   procedure Find      (S       : in  Step_File_Type;
                        Stanza  : in  Step_Handle;
                        Log     : in  Logger_Ptr;
                        Result  : in  Find_Result_Procedure);
   procedure Finalize  (S       : in out Step_File_Type);


private  ----------------------------------------------------------------------

   type Pattern_Matcher_Ptr is                  --  GCOV_IGNORE
      access all GNAT.Regpat.Pattern_Matcher;   --  GCOV_IGNORE

   type Step_Definition_Type is new Ada.Finalization.Controlled with
      record
         Prefix    : Step_Kind;
         Pattern_R : Pattern_Matcher_Ptr;
         Pattern_S : Unbounded_String;
         Proc_Name : Unbounded_String;
         Position  : Position_Type;
      end record;

   procedure Initialize (Object : in out Step_Definition_Type);
   procedure Adjust     (Object : in out Step_Definition_Type);
   procedure Finalize   (Object : in out Step_Definition_Type);

   package Step_Container is new
      Ada.Containers.Vectors (Natural, Step_Definition_Type);

   type Step_File_Type is abstract new Reffy.Counted_Type with
      record
         Parsed     : Boolean := False;
         File_Name  : Unbounded_String;
         Steps      : Step_Container.Vector;
      end record;

end XReq.Step_Definitions;
