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

package XReq.Step_Definitions.Handles is

   package Handles_Pkg is
      new Reffy.Handles (Step_File_List_Type, Step_File_List_Ptr);

   subtype Step_File_List_Handle is Handles_Pkg.Handle;

   function Create return Step_File_List_Handle;

   subtype Step_Match_Type is XReq.Step_Definitions.Step_Match_Type;
   package Step_Match_Vectors renames XReq.Step_Definitions.Match_Vectors;
   subtype Step_Match_Location is XReq.Step_Definitions.Match_Location;

end XReq.Step_Definitions.Handles;
