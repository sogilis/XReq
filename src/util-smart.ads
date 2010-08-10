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
   with procedure Initialize (Val : in out T) is null;
   with procedure Finalize   (Val : in out T) is null;

package Util.Smart is

   --  Ptr is a smart pointer. It points to a data element initialized by the
   --  new operator. This data element contains the limited type T and a
   --  reference count.
   --
   --  When initialized, the smart pointer automatically creates this data type
   --  with default values. You can change the associated datatype with the
   --  procedure Set which will take another pointer and will reference the
   --  same data type.
   --
   --  Make creates a new data type and associates it with the smart pointer.
   --  The data type takes the value given.
   --
   --  Val retrieves the data type found in the data type pointed by the smart
   --  pointer. It raises a Constraint_Error if the smart pointer is null.
   --
   --  UnRef makes the smart pointer null.
   --
   --  Is_Null test that the smart pointer is null. Valid and Is_Valid test
   --  that the smart pointer is not null.
   --

   type Ptr is new Ada.Finalization.Controlled with private;

   function  Val      (P : in     Ptr) return T;
   procedure Set      (P : in out Ptr; Val : in Ptr);
   procedure Make     (P : in out Ptr; Val : in T);
   procedure UnRef    (P : in out Ptr);
   function  Is_Null  (P : in     Ptr) return Boolean;
   function  Valid    (P : in     Ptr) return Boolean;
   function  Is_Valid (P : in     Ptr) return Boolean renames Valid;

   --------------------
   --  Guru Section  --
   --------------------
   --  IncRef and DecRef manually increases and decreases the reference count.
   --  Ref return the reference count. These shouldn't be used in normal usage.

   overriding procedure Initialize (P : in out Ptr);
   overriding procedure Adjust     (P : in out Ptr);
   overriding procedure Finalize   (P : in out Ptr);

   procedure IncRef   (P : in out Ptr);
   procedure DecRef   (P : in out Ptr);
   function  Ref      (P : in     Ptr) return Natural;

private

   type Data_Type is
      record
         Value : T;
         Refs  : Integer := 0;
      end record;

   type Data_Ptr  is access all Data_Type;

   type Ptr is new Ada.Finalization.Controlled with
      record
         Pointer : Data_Ptr := null;
      end record;

end Util.Smart;
