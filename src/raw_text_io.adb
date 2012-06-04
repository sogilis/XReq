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

package body Raw_Text_IO is

   procedure Get
     (File   : in Ada.Text_IO.File_Type;
      Buffer : in out String;
      Last   : out Natural)
   is
      use Ada.Text_IO;
      C : Character;
      I : Integer := Buffer'First;
   begin
      loop
         Get_Immediate (File, C);
         Buffer (I) := C;
         I := I + 1;
      end loop;
   exception
      when End_Error =>
         Last := I - 1;
   end Get;

   function Get_Contents
     (File : in Ada.Text_IO.File_Type)
      return String
   is
      use Ada.Text_IO;
      Buffer : String (1 .. 1024);
      Last   : Natural;

      function Get_Rest (S : String) return String;
      --  This is a recursive function that reads the rest of the file and
      --  returns it. S is the part read so far.

      --------------
      -- Get_Rest --
      --------------

      function Get_Rest (S : String) return String is

         --  Each time we allocate a buffer the same size as what we have
         --  read so far. This limits us to a logarithmic number of calls
         --  to Get_Rest and also ensures only a linear use of stack space.

         Buffer : String (1 .. S'Length);
         Last   : Natural;

      begin
         Get (File, Buffer, Last);

         if Last < Buffer'Last then
            return S & Buffer (1 .. Last);
         else
            return Get_Rest (S & Buffer (1 .. Last));
         end if;
      end Get_Rest;

   begin
      Get (File, Buffer, Last);

      if Last < Buffer'Last then
         return Buffer (1 .. Last);
      else
         return Get_Rest (Buffer (1 .. Last));
      end if;
   end Get_Contents;

end Raw_Text_IO;
