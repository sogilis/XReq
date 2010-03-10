--                         Copyright (C) 2010, Sogilis                       --

with Ada.Containers.Hashed_Sets;
with Ada.Strings.Unbounded.Hash;

package Util.Strings.Pool is

   -------------------
   --  String_Pool  --
   -------------------

   type String_Pool is private;

   Empty_Pool : constant String_Pool;

   procedure Add_Pool          (Pool   : in out String_Pool;
                                Str    : in     String);
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

   Empty_Pool : constant String_Pool := (others => <>);

end Util.Strings.Pool;
