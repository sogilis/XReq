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
with Util.IO;
with Util.Strings;
with XReq.Steps.Result.Handles;
with XReq.Scenarios.Handles;
with XReq.Step_Definition_List.Handles;

use Util.IO;
use Util.Strings;
use XReq.Steps.Result.Handles;
use XReq.Scenarios.Handles;
use XReq.Step_Definition_List.Handles;

package XReq.Scenarios.Result is

   ----------------------------
   --  Result_Scenario_Type  --
   ----------------------------

   --  Contain a list of procedure names matching the step definitions along
   --  with their parameters.

   type Result_Scenario_Type is new XReq.Scenarios.Scenario_Type with private;
   type Result_Scenario_Ptr is access all Result_Scenario_Type'Class;

   --  Creation  --------------------------------------------------------------

   procedure Make             (Res           : out    Result_Scenario_Type;
                               Scenario      : in     Scenario_Handle);

   --  Processing  ------------------------------------------------------------

   function  To_Code          (Res           : in     Result_Scenario_Type;
                               Indent        : in     String := "")
                                               return String;
   procedure Process_Scenario (Res           : in out Result_Scenario_Type;
                               Scenario      : in     Scenario_Handle;
                               Steps         : in     Step_File_List_Handle;
                               Log           : in     Logger_Ptr;
                               Errors        : out    Boolean;
                               Missing_Steps : in out String_Set;
                               Step_Matching : in     Boolean := False);

   --  Collection: Outlines  --------------------------------------------------

   function  Outline_First (S : in Result_Scenario_Type) return Natural;
   function  Outline_Last  (S : in Result_Scenario_Type) return Integer;
   function  Outline_Count (S : in Result_Scenario_Type) return Natural;
   function  Outline_Step_First   (S       : in Result_Scenario_Type;
                                   Outline : in Natural)  return Natural;
   function  Outline_Step_Last    (S       : in Result_Scenario_Type;
                                   Outline : in Natural)  return Integer;
   function  Outline_Step_Count   (S       : in Result_Scenario_Type;
                                   Outline : in Natural)  return Natural;
   function  Outline_Step_Element (S       : in Result_Scenario_Type;
                                   Outline : in Natural;
                                   Step    : in Natural)
                                   return Result_Step_Handle;

   --  Inherited Collection: Steps  -------------------------------------------

   procedure Step_Append  (Scenario : in out Result_Scenario_Type;
                           Stanza   : in     Result_Step_Handle);
   function  Step_Element (Scenario : in     Result_Scenario_Type;
                           Index    : in     Natural)
                                      return Result_Step_Handle;
   ----------------------------------------------------------------------------

private

   use XReq.Steps.Result.Handles.Handles_Pkg;
   package Result_Steps is new Ada.Containers.Vectors
      (Natural, Result_Step_Handle, "=");

   package Result_Steps_Vectors2 is
      new Ada.Containers.Vectors
         (Natural, Result_Steps.Vector, Result_Steps."=");

   type Result_Scenario_Type is new XReq.Scenarios.Scenario_Type with
      record
         Scenarios : Result_Steps_Vectors2.Vector;
      end record;

end XReq.Scenarios.Result;
