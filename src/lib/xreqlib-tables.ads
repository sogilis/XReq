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

with Ada.Containers.Ordered_Maps;

generic

   type Element_Type is private;
   with function "=" (Left, Right : in Element_Type) return Boolean is <>;

package XReqLib.Tables is

   --  GCOV_IGNORE_BEGIN
   --  Cannot cover generic package

   type Key_Type is
      record
         X : Integer;
         Y : Integer;
      end record;

   function "<" (Left, Right : in Key_Type) return Boolean;

   type Table is tagged private;
   type Cursor is private;

   function Length   (T    : in Table) return Key_Type;
   function Is_Empty (T    : in Table) return Boolean;
   function Count    (T    : in Table) return Natural;
   function First_X  (T    : in Table) return Integer;
   function First_Y  (T    : in Table) return Integer;
   function Last_X   (T    : in Table) return Integer;
   function Last_Y   (T    : in Table) return Integer;
   function Length_X (T    : in Table) return Integer;
   function Length_Y (T    : in Table) return Integer;
   function Item     (T    : in Table;
                      X, Y : in Integer) return Element_Type;

   procedure Recompute_Boundaries
                     (T    : in out Table);
   procedure Clear   (T    : in out Table);
   procedure Put     (T    : in out Table;
                      X, Y : in     Integer;
                      Elem : in     Element_Type);
   procedure Item    (T    : in     Table;
                      X, Y : in     Integer;
                      Elem : out    Element_Type;
                      Ok   : out    Boolean);
   procedure Remove  (T    : in out Table;
                      X, Y : in     Integer;
                      Recompute : in Boolean := True);
   procedure First_X (T    : in out Table; X : in Integer);
   procedure First_Y (T    : in out Table; Y : in Integer);
   procedure Last_X  (T    : in out Table; X : in Integer);
   procedure Last_Y  (T    : in out Table; X : in Integer);
   procedure Add_X   (T    : in out Table);
   procedure Add_Y   (T    : in out Table);

   function  First       (T : in     Table) return Cursor;
   procedure Next        (C : in out Cursor);
   function  Element     (C : in     Cursor) return Element_Type;
   function  Key         (C : in     Cursor) return Key_Type;
   function  Has_Element (C : in     Cursor) return Boolean;

--    Empty_Table : constant Table := <>;

   function "=" (Left, Right : in Table) return Boolean;

private

   package Maps is
      new Ada.Containers.Ordered_Maps (Key_Type, Element_Type, "<", "=");

   type Table is tagged
      record
         Map : Maps.Map;
         First_X : Integer :=  0;
         First_Y : Integer :=  0;
         Last_X  : Integer := -1;
         Last_Y  : Integer := -1;
      end record;

   type Cursor is
      record
         C : Maps.Cursor;
      end record;

end XReqLib.Tables;
