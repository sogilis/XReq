--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with AdaSpecLib.Tables;

use Ada.Strings.Unbounded;

package AdaSpecLib.String_Tables is

   package Unbounded_String_Tables is new AdaSpecLib.Tables
      (Unbounded_String, "=");

   subtype Element_Type is Unbounded_String;
   subtype Key_Type     is Unbounded_String_Tables.Key_Type;

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
   function  Element (C    : in     Cursor) return String;




   procedure Next        (C : in out Cursor)
      renames Unbounded_String_Tables.Next;
   function  Element     (C : in     Cursor) return Element_Type
      renames Unbounded_String_Tables.Element;
   function  Key         (C : in     Cursor) return Key_Type
      renames Unbounded_String_Tables.Key;
   function  Has_Element (C : in     Cursor) return Boolean
      renames Unbounded_String_Tables.Has_Element;

end AdaSpecLib.String_Tables;

