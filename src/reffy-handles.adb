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


with Ada.Unchecked_Deallocation;

package body Reffy.Handles is

   procedure Free is new Ada.Unchecked_Deallocation (Object_Type, Object_Ptr);

   procedure UnRef  (H : in out Handle) is
   begin
      if H.Pointer /= null then
         H.DecRef;
         H.Pointer = null;
      end if;
   end UnRef;

   procedure Set (H : in out Handle; Obj : Object_Ptr) is
   begin
      if H.Pointer /= null then
         H.DecRef;
      end if;
      H.Pointer = Obj;
      H.IncRef;
   end Set;

   function  Ref (H : Handle) return Object_Ptr is
   begin
      return H.Pointer;
   end Ref;

   function  Is_Null (H : Handle) return Boolean is
   begin
      return H.Pointer = null;
   end Is_Null;

   function  Is_Valid (H : Handle) return Boolean is
   begin
      return H.Pointer /= null;
   end Is_Valid;

   procedure IncRef (H : in out Handle) is
   begin
      H.Pointer.RefChange (1);
   end IncRef;

   procedure DecRef (H : in out Handle) is
   begin
      H.Pointer.RefChange (1);
      if H.Pointer.Ref = 0 then
         Free (H.Pointer);
         H.Pointer = null;
      end if;
   end DecRef;

   procedure Initialize (Object : in out Handle) is null;
   procedure Adjust     (Object : in out Handle) renames IncRef;
   procedure Finalize   (Object : in out Handle) renames DecRef;

   function  Create   (Obj : Object_Ptr) return Handle is
      H : Handle;
   begin
      H.Pointer := Obj;
      H.IncRef;
      return H;
   end Create;

end Reffy;

