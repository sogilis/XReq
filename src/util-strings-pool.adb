--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Fixed;

package body Util.Strings.Pool is

   -------------------------
   --  Get_Unique_String  --
   -------------------------

   procedure Get_Unique_String (Pool   : in out String_Pool;
                                Base   : in     String;
                                Result : out    Unbounded_String)
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use String_Pools;
      I : Natural := 2;
      S : Unbounded_String;
   begin
      S := To_Unbounded_String (Base);
      while Pool.Set.Contains (S) loop
         S := To_Unbounded_String (Base) & "_" & Trim (I'Img, Side => Left);
         I := I + 1;
      end loop;
      Pool.Set.Insert (S);
      Result := S;
   end Get_Unique_String;

end Util.Strings.Pool;
