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
with XReqLib;
with XReqLib.Generic_Scenarios;
with Util.IO;
with Util.Strings;
with XReq.Result_Steps;
with XReq.Scenarios;
with XReq.Step_Definitions;

use XReqLib;
use Util.IO;
use Util.Strings;
use XReq.Result_Steps;
use XReq.Scenarios;
use XReq.Step_Definitions;

package XReq.Result_Scenarios is

   ----------------------------
   --  Result_Scenario_Type  --
   ----------------------------

   --  Contain a list of procedure names matching the step definitions along
   --  with their parameters.

   package Scenarios_Package is new XReqLib.Generic_Scenarios
      (Result_Step_Type, XReq.Result_Steps.Equals);

   type Result_Scenario_Type is new
     Scenarios_Package.Scenario_Type with private;

   --  Creation  --------------------------------------------------------------

   procedure Make             (Res           : out    Result_Scenario_Type;
                               Scenario      : in     Scenario_Type);

   --  Processing  ------------------------------------------------------------

   function  To_Code          (Res           : in     Result_Scenario_Type;
                               Indent        : in     String := "")
                                               return String;
   procedure Process_Scenario (Res           : out    Result_Scenario_Type;
                               Scenario      : in     Scenario_Type;
                               Steps         : in     Step_Definitions_Type;
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
                                   return Result_Step_Type;

   ----------------------------------------------------------------------------

private

   package Result_Steps is new Ada.Containers.Vectors
      (Natural, Result_Step_Type, XReq.Result_Steps.Equals);

   package Result_Steps_Vectors2 is
      new Ada.Containers.Vectors
         (Natural, Result_Steps.Vector, Result_Steps."=");

   type Result_Scenario_Type is new Scenarios_Package.Scenario_Type with
      record
         Scenarios : Result_Steps_Vectors2.Vector;
      end record;

end XReq.Result_Scenarios;
