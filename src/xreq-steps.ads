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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with XReq.Args;
with XReqLib;
with Reffy;

use Ada.Strings.Unbounded;
use XReq.Args;
use XReqLib;

package XReq.Steps is

   -------------------
   --  Stanza_Type  --
   -------------------

   type Step_Type is new Reffy.Counted_Type with private;  --  GCOV_IGNORE
   type Step_Ptr is access all Step_Type'Class;

   --  Creation  --------------------------------------------------------------

   procedure Make         (Step     : in out Step_Type;
                           Kind     : in  Step_Kind;
                           Stanza   : in  String;
                           Position : in  Position_Type);

   procedure Make         (Step     : in out Step_Type;
                           Other    : in     Step_Type);

   --  Processing  ------------------------------------------------------------

   function  To_String    (S : in Step_Type;
                           K : in Step_All_Kind := Step_Null) return String;
   function  To_Regexp    (S : in Step_Type)                  return String;

   --  Properties: Read  ------------------------------------------------------

   function  Position     (S : in Step_Type) return Position_Type;
   function  Stanza       (S : in Step_Type) return String;
   function  Kind         (S : in Step_Type) return Step_Kind;

   --  Properties: Write  -----------------------------------------------------

   procedure Set_Position (S      : in out Step_Type;
                           Pos    : in     Position_Type);
   procedure Set_Stanza   (S      : in out Step_Type;
                           Stanza : in     String);
   procedure Set_Kind     (S      : in out Step_Type;
                           Kind   : in     Step_Kind);

   --  Collection: Arguments  -------------------------------------------------

   function  Arg_First    (S : in     Step_Type)     return Natural;
   function  Arg_Last     (S : in     Step_Type)     return Integer;
   function  Arg_Count    (S : in     Step_Type)     return Natural;
   function  Arg_Element  (S : in     Step_Type;
                           I : in     Natural)       return Argument_Type;
   procedure Arg_Append   (S : in out Step_Type;
                           E : in     Argument_Type);

   ----------------------------------------------------------------------------

   function Equals (Left, Right : in Step_Type) return Boolean;

   Null_Step : constant Step_Type;

   ----------------------------------------------------------------------------

private

   package Argument_Vectors is new
      Ada.Containers.Vectors (Natural, Argument_Type, "=");

   type Step_Type is new Reffy.Counted_Type with
      record
         Prefix   : Step_Kind;
         M_Stanza : Unbounded_String;
         Args     : Argument_Vectors.Vector;
         Pos      : Position_Type;
      end record;

   Null_Step : constant Step_Type := (Reffy.Counted_Type with others => <>);

end XReq.Steps;
