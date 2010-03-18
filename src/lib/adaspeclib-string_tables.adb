--                         Copyright (C) 2010, Sogilis                       --

package body AdaSpecLib.String_Tables is

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
   end Item;

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


end AdaSpecLib.String_Tables;
