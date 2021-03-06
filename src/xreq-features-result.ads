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

with Util.Strings;
with Util.IO;
with XReq.Scenarios.Result;
with XReq.Features;
with XReq.Features.Handles;
with XReq.Step_Definition_List.Handles;
with XReq.Scenarios.Result.Handles;

use Util.Strings;
use Util.IO;
use XReq.Scenarios.Result;
use XReq.Features;
use XReq.Features.Handles;
use XReq.Step_Definition_List.Handles;
use XReq.Scenarios.Result.Handles;


package XReq.Features.Result is

   ---------------------------
   --  Result_Feature_Type  --
   ---------------------------

   --  The Result_Feature_Type describes the result of the combinaison of the
   --  steps in the feature file and the step definitions. It contains the
   --  procedures of the step definitions to call in correct order and their
   --  parameters.

   type Result_Feature_Type is new Feature_Type with private;
   type Result_Feature_Ptr is access all Result_Feature_Type'Class;

   --  Processing  ------------------------------------------------------------

   function  To_Code         (Res           : in     Result_Feature_Type;
                              Indent        : in     String := "")
                                              return String;
   procedure Process_Feature (Res           : in out Result_Feature_Type;
                              Feature       : in     Feature_Handle;
                              Steps         : in     Step_File_List_Handle;
                              Log           : in     Logger_Ptr;
                              Missing_Steps : in out String_Set;
                              Step_Matching : in     Boolean := False);

   --  Properties  ------------------------------------------------------------

   function  Fail           (F    : in     Result_Feature_Type) return Boolean;
   function  Background     (F    : in     Result_Feature_Type)
                                    return Result_Scenario_Handle;

   procedure Set_Fail       (F    : in out Result_Feature_Type;
                             Fail : in     Boolean := True);
   procedure Set_Background (F    : in out Result_Feature_Type;
                             Bg   : in     Result_Scenario_Handle);

   --  Inbherited Collection: Scenario  ---------------------------------------

   function  Scenario_Element   (F : in     Result_Feature_Type;
                                 I : in     Natural)
                                     return Result_Scenario_Handle;
   procedure Scenario_Append    (F : in out Result_Feature_Type;
                                 S : in     Result_Scenario_Handle);

   ----------------------------------------------------------------------------

private

   type Result_Feature_Type is new Feature_Type with
      record
         Fail : Boolean       := False;
      end record;

end XReq.Features.Result;
