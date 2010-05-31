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

with Ada.Text_IO;

use Ada.Text_IO;

package body AUnit_Reporter is

   overriding procedure Report (Engine : in Reporter;
                                R  : in out AUnit.Test_Results.Result'Class)
   is
      Old_Output : constant File_Type := Current_Output;
      Old_Error  : constant File_Type := Current_Error;
   begin

      Set_Output (Engine.File.all);
      Set_Error  (Engine.File.all);

      GNAT.IO.Set_Output (Engine.GNAT_IO.all);

      Engine.Reporter.all.Report (R);

      Set_Output (Old_Output);
      Set_Error  (Old_Error);

   end Report;

end AUnit_Reporter;
