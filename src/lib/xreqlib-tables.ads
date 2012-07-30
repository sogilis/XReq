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

   type Table_Header_Kind is (None, First_Row, First_Column, Transpose);

   type Table_Data_Set is new Integer range 0 .. Integer'Last;

   First_Table_Data_Set : constant Table_Data_Set := 1;

   type Key_Type is
      record
         X : Integer;
         Y : Integer;
      end record;

   function "<" (Left, Right : in Key_Type) return Boolean;

   type Table is tagged private;
   type Cursor is private;

   ----------------------------------------------------------------------------
   ----------------------------  Basic Operations  ----------------------------
   ----------------------------------------------------------------------------

   function  Is_Empty  (T : in     Table) return Boolean;
   function  Count     (T : in     Table) return Natural;
   function  Is_Sparse (T : in     Table) return Boolean;
   function  Transpose (T : in     Table) return Table;
   procedure Clear     (T : in out Table);

   ----------------------------------------------------------------------------
   -----------------  Iterate over the elements of the table  -----------------
   ----------------------------------------------------------------------------

   function  First       (T : in     Table) return Cursor;
   procedure Next        (C : in out Cursor);
   function  Element     (C : in     Cursor) return Element_Type;
   function  Key         (C : in     Cursor) return Key_Type;
   function  Has_Element (C : in     Cursor) return Boolean;

   ----------------------------------------------------------------------------
   ----------------------  Table by X and Y Coordinates  ----------------------
   ----------------------------------------------------------------------------

   --  Table with items indexed by X, Y coordinates

   function Length   (T : in Table) return Key_Type;
   function First_X  (T : in Table) return Integer;
   function First_Y  (T : in Table) return Integer;
   function Last_X   (T : in Table) return Integer;
   function Last_Y   (T : in Table) return Integer;
   function Length_X (T : in Table) return Integer;
   function Length_Y (T : in Table) return Integer;

   --  Change the structure of the table

   procedure First_X (T    : in out Table; X : in Integer);
   procedure First_Y (T    : in out Table; Y : in Integer);
   procedure Last_X  (T    : in out Table; X : in Integer);
   procedure Last_Y  (T    : in out Table; X : in Integer);
   procedure Add_X   (T    : in out Table);
   procedure Add_Y   (T    : in out Table);

   procedure Recompute_Boundaries
     (T : in out Table);
   --  Recompute_Boundaries: recompute first and last X and Y based on the
   --  elements of the table.

   function Item
     (T    : in Table;
      X, Y : in Integer)
      return Element_Type;
   --  Item: get item by X and Y coordinates or raise Constraint_Error

   procedure Item
     (T    : in  Table;
      X, Y : in  Integer;
      Elem : out Element_Type;
      Ok   : out Boolean);
   --  Item: get item by X and Y coordinates or Ok is False

   function  Item
     (T       : in Table;
      X, Y    : in Integer;
      Default : in Element_Type)
      return Element_Type;
   --  Item: get item by X and Y coordinates or return Default

   function Has_Item
     (T       : in Table;
      X, Y    : in Integer)
      return Boolean;

   procedure Put
     (T    : in out Table;
      X, Y : in     Integer;
      Elem : in     Element_Type);
   --  Put: insert an item in the table by its X and Y coordinates

   procedure Remove
     (T    : in out Table;
      X, Y : in     Integer;
      Recompute : in Boolean := True);
   --  Remove: remove an item by its X and Y coordinates.
   --  Recompute must be True or it may lead to inconsistencies.

   ----------------------------------------------------------------------------
   ------------------------------  Table Header  ------------------------------
   ----------------------------------------------------------------------------

   function  Header_Kind
     (T : in     Table)
      return Table_Header_Kind;

   procedure Set_Header_Kind
     (T : in out Table;
      H : in     Table_Header_Kind := None);

   ----------------------------------------------------------------------------
   ----------------------  Table by Data Set and Record  ----------------------
   ----------------------------------------------------------------------------

   procedure Data_To_XY
     (T    : in  Table;
      DS   : in  Table_Data_Set;
      Rec  : in  Integer;
      X    : out Integer;
      Y    : out Integer);

   procedure XY_To_Data
     (T    : in  Table;
      X    : in  Integer;
      Y    : in  Integer;
      DS   : out Table_Data_Set;
      Rec  : out Integer);

   function  Data_Set_For
     (T : in Table;
      H : in Element_Type)
      return Table_Data_Set;
   --  Data_Set_For: Find the data set index given the header content


   function  Data_Sets_Count (T : in Table) return Natural;
   function  First_Data_Set  (T : in Table) return Table_Data_Set;
   function  Last_Data_Set   (T : in Table) return Table_Data_Set;
   function  Next_Data_Set   (T : in Table) return Table_Data_Set;

   function  Records_Count   (T : in Table) return Natural;
   function  Head_Record     (T : in Table) return Natural;
   function  First_Record    (T : in Table) return Positive;
   function  Last_Record     (T : in Table) return Natural;
   function  Next_Record     (T : in Table) return Positive;

   function  Get_Record
     (T : Table;
      D : Table_Data_Set;
      R : Natural)
      return Element_Type;

   function  Get_Record
     (T       : in Table;
      D       : in Table_Data_Set;
      R       : in Natural;
      Default : in Element_Type)
      return Element_Type;

   procedure Get_Record
     (T    : in  Table;
      D    : in  Table_Data_Set;
      R    : in  Natural;
      Elem : out Element_Type;
      Ok   : out Boolean);

   function Has_Record
     (T    : in  Table;
      D    : in  Table_Data_Set;
      R    : in  Natural)
      return Boolean;

   procedure Remove_Record
     (T    : in out Table;
      D    : in     Table_Data_Set;
      R    : in     Natural);

   procedure Set_Record
     (T : in out Table;
      D : in     Table_Data_Set;
      R : in     Natural;
      E : in     Element_Type);

   procedure Set_Header_Name
     (T                      : in out Table;
      Old_Header, New_Header : Element_Type);
   --  Replace a header by another

   procedure Import_Data_Set
     (T : in out Table;
      Other_Table : in Table;
      Other_Header : Element_Type;
      Rename       : Element_Type);
   --  Import a data set from another table

   type Comparison_Failure_Type is
     (Fail_Sparse, Fail_Num_Records, Fail_Missing_Header, Fail_Cell);

   procedure Compare
     (T                      : in  Table;
      Other                  : in  Table;
      Ignore_Missing_Headers : in  Boolean := False;
      Result                 : out Boolean;
      Reason                 : out Comparison_Failure_Type;
      DataSet1               : out Table_Data_Set;
      DataSet2               : out Table_Data_Set;
      Rec                    : out Natural);

   ----------------------------------------------------------------------------
   ----------------------------  Other Operations  ----------------------------
   ----------------------------------------------------------------------------

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
         Head    : Table_Header_Kind := None;
      end record;

   type Cursor is
      record
         C : Maps.Cursor;
      end record;

end XReqLib.Tables;
