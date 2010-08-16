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
with Util.IO;

use Ada.Strings.Unbounded;
use Util.IO;

package XReq.Features.Files is

   -------------------------
   --  Feature_File_Type  --
   -------------------------

   type Feature_File_Type is new Generic_Feature_Type with private;
   type Feature_File_Ptr  is access all Feature_File_Type'Class;

   procedure Make      (F         : out    Feature_File_Type;
                        File_Name : in     String);
   function  Create    (File_Name : in     String) return Feature_File_Type;
   function  File_Name (F         : in     Feature_File_Type) return String;
   procedure Parse     (F         : in out Feature_File_Type;
                        Log       : in     Logger_Ptr);

   overriding
   function  Parsed    (F         : in     Feature_File_Type) return Boolean;
   overriding
   function  To_String (F         : in     Feature_File_Type) return String;


   Null_Feature_File : constant Feature_File_Type;

private  ----------------------------------------------------------------------

   type Feature_File_Type is new Generic_Feature_Type with
      record
         Parsed      : Boolean := False;
         File_Name   : Unbounded_String;
      end record;

   Null_Feature_File : constant Feature_File_Type :=
      Feature_File_Type'(Null_Feature with others => <>);

end XReq.Features.Files;

