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
with XReqLib;
with XReqLib.String_Tables;
with XReqLib.Interface_Scenarios;
with XReq.Steps;
with XReq.Steps.Handles;

use Ada.Strings.Unbounded;
use XReqLib;
use XReqLib.Interface_Scenarios;
use XReq.Steps.Handles;

package XReq.Scenarios is

   subtype Table_Type is XReqLib.String_Tables.Table;

   ---------------------
   --  Scenario_Type  --
   ---------------------

   type Scenario_Type is new Scenario_Interface with private;
   type Scenario_Ptr is access all Scenario_Type'Class;

   --  Creation  --------------------------------------------------------------

   procedure Make         (Scenario : out    Scenario_Type;
                           Name     : in     String;
                           Position : in     Position_Type := Null_Position;
                           Outline  : in     Boolean := False;
                           Tags     : in     String_Vector :=
                                             String_Vectors.Empty_Vector);

   --  Processing  ------------------------------------------------------------

   procedure Output_Steps (S     : in     Scenario_Type;
                           Buf   : in out Unbounded_String);

   --  Properties: Read  ------------------------------------------------------

   function  Outline      (S : in Scenario_Type) return Boolean;
   function  Name         (S : in Scenario_Type) return String;
   function  Position     (S : in Scenario_Type) return Position_Type;
   function  Tag_Vector   (S : in Scenario_Type) return String_Vector;
   function  Table        (S : in Scenario_Type)
                           return XReqLib.String_Tables.Table;

   --  Properties: Write  -----------------------------------------------------

   procedure Set_Name     (S     : in out Scenario_Type;
                           Name  : in     String);

   procedure Set_Table    (S     : in out Scenario_Type;
                           Table : in     XReqLib.String_Tables.Table);

   --  Collection: Steps  -----------------------------------------------------

   procedure Step_Append  (Scenario : in out Scenario_Type;
                           Stanza   : in     Step_Handle);
   function  Step_First   (Scenario : in     Scenario_Type) return Natural;
   function  Step_Last    (Scenario : in     Scenario_Type) return Integer;
   function  Step_Count   (Scenario : in     Scenario_Type) return Natural;
   function  Step_Element (Scenario : in     Scenario_Type;
                           Index    : in     Natural)       return Step_Handle;

   --  Collection: Tags  ------------------------------------------------------

   function  Tag_First    (Scenario : in     Scenario_Type) return Natural;
   function  Tag_Last     (Scenario : in     Scenario_Type) return Integer;
   function  Tag_Count    (Scenario : in     Scenario_Type) return Natural;
   function  Tag_Element  (Scenario : in     Scenario_Type;
                           Index    : in     Natural)       return String;

   ----------------------------------------------------------------------------

   function Equals (Left, Right : in Scenario_Type) return Boolean;

   Null_Scenario         : constant Scenario_Type;
   Null_Scenario_Outline : constant Scenario_Type;

private

   package Step_Vectors is new Ada.Containers.Vectors
     (Natural, Step_Handle, Steps.Handles.Handles_Pkg."=");

   type Scenario_Record (Outline : Boolean := False) is
      record
         Name          : Unbounded_String;
         Pos           : Position_Type;
         Tags          : String_Vector;
         Steps         : Step_Vectors.Vector;
         --  GCOV_IGNORE_BEGIN
         case Outline is
            when True =>
               Table   : XReqLib.String_Tables.Table;
            when False =>
               null;
         end case;
         --  GCOV_IGNORE_END
      end record;

   type Scenario_Type is new Scenario_Interface with
      record
         D : Scenario_Record;
      end record;

   Null_Scenario         : constant Scenario_Type := (others => <>);
   Null_Scenario_Outline : constant Scenario_Type :=
     (D => Scenario_Record'(Outline => True, others => <>));

end XReq.Scenarios;
