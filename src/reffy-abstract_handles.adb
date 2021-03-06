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


with System;
with System.Address_Image;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions;

package body Reffy.Abstract_Handles is


   procedure Log (H : Handle; Msg : String);
   function  Ptr (Obj : Object_Ptr) return String;
   procedure Free is
      new Ada.Unchecked_Deallocation (Object_Type'Class, Object_Ptr);


   function  Ptr (Obj : Object_Ptr) return String is
   begin
      if Obj = null then
         return System.Address_Image (System.Null_Address);
      else
         return System.Address_Image (Obj.all'Address);
      end if;
   end Ptr;

   procedure Log (H : Handle; Msg : String) is
      use Ada.Text_IO;
   begin
      if not Traces then return; end if;
      Put ("[Reffy.Handles] " & System.Address_Image (H'Address) &
           " -> " & Ptr (H.Pointer));
      if H.Pointer /= null then
         Put (H.Pointer.Ref'Img);
      end if;
      Put_Line (" " & Msg);
   end Log;

   ----------------------------------------------------------------------------

   procedure IncRef (H : in out Handle) is
      use Ada.Exceptions;
      use Ada.Text_IO;
   begin
      Log (H, "IncRef");
      H.Pointer.RefChange (1);
   exception
      when E : others =>
         Put_Line ("Reffy: Error while IncRef");
         Put_Line (Exception_Information (E));
   end IncRef;

   procedure DecRef (H : in out Handle) is
      use Ada.Exceptions;
      use Ada.Text_IO;
   begin
      Log (H, "DecRef");
      H.Pointer.RefChange (-1);
      if H.Pointer.Ref = 0 then
         Log (H, "Free (DecRef)");
         Free (H.Pointer);
         H.Pointer := null;
      end if;
   exception
      when E : others =>
         Put_Line ("Reffy: Error while DecRef");
         Put_Line (Exception_Information (E));
   end DecRef;

   procedure Initialize (Object : in out Handle) is
   begin
      Log (Object, "Initialize");
   end Initialize;

   procedure Adjust     (Object : in out Handle) is
   begin
      Log (Object, "Adjust");
      if Object.Pointer /= null then
         Object.IncRef;
      end if;
   end Adjust;

   procedure Finalize   (Object : in out Handle) is
   begin
      Log (Object, "Finalize");
      if Object.Pointer /= null then
         Object.DecRef;
      end if;
   end Finalize;

   procedure UnRef  (H : in out Handle) is
   begin
      Log (H, "UnRef");
      if H.Pointer /= null then
         H.DecRef;
         H.Pointer := null;
      end if;
   end UnRef;

   procedure Set (H : in out Handle; Obj : Object_Ptr) is
   begin
      Log (H, "Set " & Ptr (Obj));
      if H.Pointer /= null then
         H.DecRef;
      end if;
      H.Pointer := Obj;
      if Obj /= null then
         H.IncRef;
      end if;
   end Set;

   function  Ref (H : Handle) return Object_Ptr is
   begin
      if H.Pointer = null then
         Log (H, "Ref (WARNING: NULL POINTER) " & Get_Stack_Trace);
      end if;
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

   function  Create   (Obj : Object_Ptr) return Handle is
      H : Handle;
   begin
      H.Pointer := Obj;
      Log (H, "Create");
      H.IncRef;
      return H;
   end Create;

end Reffy.Abstract_Handles;

