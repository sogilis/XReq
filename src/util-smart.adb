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
--  with Util.IO;

package body Util.Smart is

   procedure Log (S : in String) is null;
--    procedure Log (S : in String) is
--    begin
--       Util.IO.Std_Logger.Put_Line (S);
--    end Log;



   function  Val (P : in     Ptr) return T is
   begin
      Log ("Val");
      return P.Pointer.all.Value;
   end Val;

   procedure Set (P : in out Ptr; Val : in Ptr) is
   begin
      if P.Pointer /= Val.Pointer then
         P.UnRef;
         P.Pointer := Val.Pointer;
         P.IncRef;
      end if;
   end Set;

   procedure Make (P : in out Ptr; Val : in T) is
   begin
      if P.Pointer /= null then
         P.UnRef;
      end if;
      P.Pointer := new Data_Type'(Value => Val, Refs  => 1);
      Initialize (P.Pointer.all.Value);
   end Make;

   procedure IncRef (P : in out Ptr) is
   begin
      if P.Pointer = null then
         Log ("IncRef: Initialize Smart Dointer Data (ref: 1)");
         P.Pointer := new Data_Type'(Refs  => 1, others => <>);
         Initialize (P.Pointer.all.Value);
      else
         P.Pointer.all.Refs := P.Pointer.all.Refs + 1;
         Log ("IncRef (ref:" & P.Pointer.all.Refs'Img & ")");
      end if;
   end IncRef;

   procedure DecRef (P : in out Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation (Data_Type, Data_Ptr);
   begin
      if P.Pointer /= null then
         P.Pointer.all.Refs := P.Pointer.all.Refs - 1;
         Log ("DecRef (ref:" & P.Pointer.all.Refs'Img & ")");
         if P.Pointer.all.Refs <= 0 then
            Log ("Finalize Smart Pointer Data");
            Finalize (P.Pointer.all.Value);
            Free (P.Pointer);
         end if;
      end if;
   end DecRef;

   procedure UnRef   (P : in out Ptr) is
   begin
      Log ("UnRef");
      if P.Pointer /= null then
         P.DecRef;
         P.Pointer := null;
      end if;
   end UnRef;

   function  Ref    (P : in     Ptr) return Natural is
   begin
      if P.Pointer = null then
         return 0;
      else
         return P.Pointer.all.Refs;
      end if;
   end Ref;

   function  Is_Null (P : in     Ptr) return Boolean is
   begin
      return P.Pointer = null;
   end Is_Null;

   function  Valid   (P : in     Ptr) return Boolean is
   begin
      return P.Pointer /= null;
   end Valid;

   procedure Initialize (P : in out Ptr) renames IncRef;
   procedure Adjust     (P : in out Ptr) renames IncRef;
   procedure Finalize   (P : in out Ptr) renames DecRef;

end Util.Smart;
