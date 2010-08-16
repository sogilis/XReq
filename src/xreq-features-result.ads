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

with XReqLib.Generic_Features;
with Util.Strings;
with XReq.Scenarios.Result;
with XReq.Features;
with XReq.Step_Definition_List.Handles;

use Util.Strings;
use XReq.Scenarios.Result;
use XReq.Features;
use XReq.Step_Definition_List.Handles;


package XReq.Features.Result is

   ---------------------------
   --  Result_Feature_Type  --
   ---------------------------

   --  The Result_Feature_Type describes the result of the combinaison of the
   --  steps in the feature file and the step definitions. It contains the
   --  procedures of the step definitions to call in correct order and their
   --  parameters.

   package Features_Package is new XReqLib.Generic_Features
      (Result_Scenario_Type, "=");

   type Result_Feature_Type is new Features_Package.Feature_Type with private;

   --  Processing  ------------------------------------------------------------

   function  To_Code         (Res           : in     Result_Feature_Type;
                              Indent        : in     String := "")
                                              return String;
   procedure Process_Feature (Res           : out    Result_Feature_Type;
                              Feature       : in     Generic_Feature_Ptr;
                              Steps         : in     Step_File_List_Handle;
                              Log           : in     Logger_Ptr;
                              Missing_Steps : in out String_Set;
                              Step_Matching : in     Boolean := False);

   --  Properties  ------------------------------------------------------------

   function  Fail     (F    : in     Result_Feature_Type) return Boolean;
   function  Language (F    : in     Result_Feature_Type)
                              return Language_Handle;

   procedure Set_Fail (F    : in out Result_Feature_Type;
                       Fail : in     Boolean := True);

   ----------------------------------------------------------------------------

private

   type Result_Feature_Type is new Features_Package.Feature_Type with
      record
         Fail : Boolean       := False;
         Lang : Language_Handle;
      end record;

end XReq.Features.Result;
