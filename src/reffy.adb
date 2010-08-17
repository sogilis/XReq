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

with Ada.Strings.Unbounded;
with System.Address_Image;
with GNAT.Traceback;
with Ada.Text_IO;

package body Reffy is

   procedure Log (C : Counted_Type; Msg : String);
   procedure Log (C : Limited_Counted_Type; Msg : String);

   procedure Log (C : Counted_Type; Msg : String) is
      use Ada.Text_IO;
   begin
      if not Traces then return; end if;
      Put_Line ("[Reffy] " & System.Address_Image (C'Address) & C.Ref'Img &
                " " & Msg);
   end Log;

   procedure Log (C : Limited_Counted_Type; Msg : String) is
      use Ada.Text_IO;
   begin
      if not Traces then return; end if;
      Put ("[Reffy] " & System.Address_Image (C'Address) & C.Ref'Img & " " &
           Msg);
   end Log;

   --  Counted_Type  ----------------------------------------------------------

   function  "="        (A, B   :        Counted_Type) return Boolean is
      pragma Unreferenced (A, B);
   begin
      return True;
   end "=";

   function  Ref       (C :        Counted_Type) return Natural is
   begin
      return C.Ref;
   end Ref;

   procedure RefChange (C : in out Counted_Type; Inc : Integer) is
   begin
      Log (C, "RefChange " & Inc'Img);
      C.Ref := C.Ref + Inc;
      Log (C, "");
   end RefChange;

   --  Limited_Counted_Type  --------------------------------------------------

   function  "="        (A, B   :    Limited_Counted_Type) return Boolean is
      pragma Unreferenced (A, B);
   begin
      return True;
   end "=";

   function  Ref       (C :        Limited_Counted_Type) return Natural is
   begin
      return C.Ref;
   end Ref;

   procedure RefChange (C : in out Limited_Counted_Type; Inc : Integer) is
   begin
      Log (C, "RefChange " & Inc'Img);
      C.Ref := C.Ref + Inc;
      Log (C, "");
   end RefChange;

   ----------------------------------------------------------------------------

   function Get_Stack_Trace return String is
      use GNAT.Traceback;
      use Ada.Strings.Unbounded;
      Trace  : Tracebacks_Array (1 .. 1_000);
      Length : Natural;
      Buffer : Unbounded_String;
   begin
      Call_Chain (Trace, Length);
      for I in Trace'First .. Length loop
         if I /= Trace'First then Append (Buffer, " "); end if;
         Append (Buffer, "0x" & System.Address_Image (Trace (I)));
      end loop;
      return To_String (Buffer);
   end Get_Stack_Trace;

end Reffy;

