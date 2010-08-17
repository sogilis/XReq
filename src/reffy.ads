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

package Reffy is

   Traces : Boolean := False;

   --  Counted (interface)  ---------------------------------------------------

   type Counted is limited interface;

   function  Ref        (C :        Counted) return Natural is abstract;
   procedure RefChange  (C : in out Counted; Inc : Integer) is abstract;

   --  Counted_Type  ----------------------------------------------------------

   type Counted_Type is
      new Ada.Finalization.Controlled and Counted with private;

   function  "="        (A, B   :        Counted_Type) return Boolean;
   function  Ref        (C      :        Counted_Type) return Natural;
   procedure RefChange  (C      : in out Counted_Type; Inc : Integer);
   procedure Initialize (Object : in out Counted_Type) is null;
   procedure Adjust     (Object : in out Counted_Type) is null;
   procedure Finalize   (Object : in out Counted_Type) is null;

   --  Limited_Counted_Type  --------------------------------------------------

   type Limited_Counted_Type is
      new Ada.Finalization.Limited_Controlled and Counted with private;

   function  "="        (A, B   :        Limited_Counted_Type) return Boolean;
   function  Ref        (C      :        Limited_Counted_Type) return Natural;
   procedure RefChange  (C      : in out Limited_Counted_Type; Inc : Integer);
   procedure Initialize (Object : in out Limited_Counted_Type) is null;
   procedure Finalize   (Object : in out Limited_Counted_Type) is null;

private

   type Counted_Type is new Ada.Finalization.Controlled and Counted with
      record
         Ref : Natural := 0;
      end record;

   type Limited_Counted_Type is
      new Ada.Finalization.Limited_Controlled and Counted
      with record
         Ref : Natural := 0;
      end record;

   function Get_Stack_Trace return String;

end Reffy;

