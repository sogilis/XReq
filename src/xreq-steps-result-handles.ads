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

with Reffy.Handles;

package XReq.Steps.Result.Handles is

   package Handles_Pkg is new Reffy.Handles
     (Result_Step_Type, Result_Step_Ptr);

   use type Handles_Pkg.Handle;
   subtype Result_Step_Handle is Handles_Pkg.Handle;

   function Create return Result_Step_Handle;

   function Create (Step  : Step_Handle;
                    Match : Step_Match_Type := (others => <>))
                    return Result_Step_Handle;

end XReq.Steps.Result.Handles;


