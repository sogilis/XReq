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

generic

   type Object_Type (<>) is new Counted with private;
   type Object_Ptr is access Object_Type;

package Reffy.Handles is

   Traces : constant Boolean := False;

   type Handle is new Ada.Finalization.Controlled with private;

   procedure UnRef    (H : in out Handle);
   procedure Set_New  (H : in out Handle; Obj : Object_Type);
   procedure Set      (H : in out Handle; Obj : Object_Ptr);
   function  Ref      (H : Handle) return Object_Ptr;
   function  Is_Null  (H : Handle) return Boolean;
   function  Is_Valid (H : Handle) return Boolean;
   function  Valid    (H : Handle) return Boolean renames Is_Valid;

   procedure IncRef (H : in out Handle);
   procedure DecRef (H : in out Handle);

   procedure Initialize (Object : in out Handle);
   procedure Adjust     (Object : in out Handle);
   procedure Finalize   (Object : in out Handle);

   function  Create   (Obj : Object_Ptr) return Handle;

private

   type Handle is new Ada.Finalization.Controlled with
      record
         Pointer : Object_Ptr := null;
      end record;

end Reffy.Handles;

