--                         Copyright (C) 2010, Sogilis                       --

with Ada.Containers.Hashed_Sets;
with Ada.Strings.Unbounded.Hash;

package Util.Strings.Pool is

   -------------------
   --  String_Pool  --
   -------------------

   type String_Pool is private;

   procedure Get_Unique_String (Pool   : in out String_Pool;
                                Base   : in     String;
                                Result : out    Unbounded_String);

private

   package String_Pools is
      new Ada.Containers.Hashed_Sets (Unbounded_String, Hash, "=", "=");

   type String_Pool is
      record
         Set : String_Pools.Set;
      end record;

end Util.Strings.Pool;
