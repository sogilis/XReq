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

package body XReqLib.String_Tables is

   function  Width   (T    : in Table;
                      X    : in Integer) return Natural
   is
      W : Natural := 0;
   begin
      for Y in T.First_Y .. T.Last_Y loop
         W := Natural'Max (W, T.Item (X, Y, "")'Length);
      end loop;
      return W;
   end Width;

   function  Item    (T    : in Table;
                      X, Y : in Integer) return String is
   begin
      return To_String (Item (T, X, Y));
   end Item;

   function  Item    (T    : in Table;
                      X, Y : in Integer;
                      Def  : in String) return String is
   begin
      return To_String (Item (T, X, Y));
   exception
      when Constraint_Error =>
         return Def;
   end Item; --  GCOV_IGNORE

   procedure Put     (T    : in out Table;
                      X, Y : in     Integer;
                      Elem : in     String) is
   begin
      Put (T, X, Y, To_Unbounded_String (Elem));
   end Put;

   function  Element     (C : in     Cursor) return String is
   begin
      return To_String (Element (C));
   end Element;

end XReqLib.String_Tables;
