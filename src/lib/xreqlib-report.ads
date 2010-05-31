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

package XReqLib.Report is

   -------------------
   --  Report_Type  --
   -------------------

   --  GCOV_IGNORE_BEGIN
   type Report_Type is
      record
         Count_Scenario_Failed : Natural := 0;
         Count_Scenario_Passed : Natural := 0;
         Count_Steps_Failed    : Natural := 0;
         Count_Steps_Skipped   : Natural := 0;
         Count_Steps_Passed    : Natural := 0;
         Num_Steps             : Natural := 0;
      end record;
   --  GCOV_IGNORE_END

   function Status (Report : in Report_Type) return Boolean;

end XReqLib.Report;
