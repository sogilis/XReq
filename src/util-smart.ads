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

with Ada.Finalization;

generic

   type T is private;
   Null_Value : T;
   with procedure Finalize (Val : in out T) is null;

package Util.Smart is

   type Ptr is new Ada.Finalization.Controlled with private;

   function  Val      (P : in     Ptr) return T;
   procedure Set      (P : in out Ptr; Val : in T);
   procedure IncRef   (P : in out Ptr);
   procedure DecRef   (P : in out Ptr);
   procedure UnRef    (P : in out Ptr);
   function  Ref      (P : in     Ptr) return Natural;
   function  Is_Null  (P : in     Ptr) return Boolean;
   function  Valid    (P : in     Ptr) return Boolean;
   function  Is_Valid (P : in     Ptr) return Boolean renames Valid;

   overriding procedure Initialize (P : in out Ptr);
   overriding procedure Adjust     (P : in out Ptr);
   overriding procedure Finalize   (P : in out Ptr);

private

   type Data_Type is
      record
         Value : T       := Null_Value;
         Refs  : Integer := 0;
      end record;

   type Data_Ptr  is access all Data_Type;

   type Ptr is new Ada.Finalization.Controlled with
      record
         Pointer : Data_Ptr := null;
      end record;

end Util.Smart;
