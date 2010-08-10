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

with Ada.Unchecked_Deallocation;

package body Reffy is

   --  Counted_Type  ----------------------------------------------------------

   function  Ref       (C :        Counted_Type) return Natural is
   begin
      return C.Ref;
   end Ref;

   procedure RefChange (C : in out Counted_Type; Inc : Integer) is
   begin
      Ref := Ref + Inc;
   end;

   --  Limited_Counted_Type  --------------------------------------------------

   function  Ref       (C :        Limited_Counted_Type) return Natural is
   begin
      return C.Ref;
   end Ref;

   procedure RefChange (C : in out Limited_Counted_Type; Inc : Integer) is
   begin
      Ref := Ref + Inc;
   end;

end Reffy;

