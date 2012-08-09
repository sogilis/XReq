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
with XReqLib.Tables;

use Ada.Strings.Unbounded;

package XReqLib.String_Tables is

   function "+" (X : String) return Unbounded_String
                 renames To_Unbounded_String;

   function "+" (X : Unbounded_String) return String
                 renames To_String;

   package Unbounded_String_Tables is new XReqLib.Tables
      (Unbounded_String, "=");

   subtype Element_Type      is Unbounded_String;
   subtype Key_Type          is Unbounded_String_Tables.Key_Type;
   use Unbounded_String_Tables;

   function "<" (Left, Right : in Key_Type) return Boolean
      renames Unbounded_String_Tables."<";

   type    Table  is new Unbounded_String_Tables.Table with null record;
   subtype Cursor is     Unbounded_String_Tables.Cursor;

   function  Width   (T    : in Table;
                      X    : in Integer) return Natural;
   function  Item    (T    : in Table;
                      X, Y : in Integer) return String;
   function  Item    (T    : in Table;
                      X, Y : in Integer;
                      Def  : in String) return String;
   procedure Put     (T    : in out Table;
                      X, Y : in     Integer;
                      Elem : in     String);

   function  Get_Record (T   : in Table;
                         Rec : in Positive;
                         Set : in String) return String;

   procedure Set_Header_Name (T : in out Table;
                              Old_Header, New_Header : String);

   procedure Compare_With (T     : in Table;
                           Other : in Table;
                           Ignore_Missing_Headers : in Boolean := False);

   procedure Import_Data_Set (T : in out Table;
                              Other_Table : in Table;
                              Other_Header : String;
                              Rename : String);

   function  Element (C    : in     Cursor) return String;




   procedure Next        (C : in out Cursor)
      renames Unbounded_String_Tables.Next;
   function  Element     (C : in     Cursor) return Element_Type
      renames Unbounded_String_Tables.Element;
   function  Key         (C : in     Cursor) return Key_Type
      renames Unbounded_String_Tables.Key;
   function  Has_Element (C : in     Cursor) return Boolean
                          renames Unbounded_String_Tables.Has_Element;

   procedure Add_Data_Set    (T : in out Table;
                              E : in     String);
   procedure Add_Record      (T : in out Table);
   procedure Add_Data        (T : in out Table;
                              Data_Set  : in String;
                              Data      : in String;
                              Auto_Next : in Boolean := True);
   procedure Add_Data        (T : in out Table;
                              Index     : in Integer;
                              Data      : in String;
                              Auto_Next : in Boolean := True);
   procedure Sort_Data_Sets  (T : in out Table; Key : Table_Data_Set);
   procedure Sort_Data_Sets  (T : in out Table; Key : String);
   function  Data_Set_For    (T : Table; DS : String) return Table_Data_Set;

   function To_String
     (T      : Table;
      Indent : String := "";
      Index  : Boolean := True) return String;

   Comparison_Failed : exception;

end XReqLib.String_Tables;
