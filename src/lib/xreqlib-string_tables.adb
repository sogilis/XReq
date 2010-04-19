--                         Copyright (C) 2010, Sogilis                       --

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
