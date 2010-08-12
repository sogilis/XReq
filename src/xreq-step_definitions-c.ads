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

with XReq.Step_Definition_List.Handles;

use XReq.Step_Definition_List.Handles;

package XReq.Step_Definitions.C is

   --  Called in XReq.Steps.Load
   procedure Parse_Directory (Steps      : in out Step_File_List_Handle;
                              Logger     : in     Logger_Ptr;
                              Directory  : in     String;
                              Fill_Steps : in     Boolean := False);
   --  IMPORTANT: deallocate Steps_Type


   type C_Step_File_Type is new Step_File_Type with private;
   type C_Step_File_Ptr  is access all C_Step_File_Type'Class;

   procedure Make (S          : out C_Step_File_Type;
                   File_Name  : in  String;
                   Fill_Steps : in  Boolean := False);

   overriding procedure Parse     (S          : in out C_Step_File_Type;
                                   Logger     : in     Logger_Ptr);

private

   type C_Step_File_Type is new Step_File_Type with
      record
         Fill_Steps : Boolean := False;
      end record;

end XReq.Step_Definitions.C;
