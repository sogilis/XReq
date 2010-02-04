------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   A U N I T . R E P O R T E R . X M L                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2000-2008, AdaCore                   --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT is maintained by AdaCore (http://www.adacore.com)                   --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;
with AUnit.Reporter;

use Ada.Strings.Unbounded;
use AUnit.Reporter;

--  Very simple reporter to console
package AUnit.Reporter.XML2 is

   String_Result : aliased Unbounded_String;

   type XML_Reporter is new Reporter with null record;

   overriding procedure Report (Engine : in XML_Reporter;
                                R      : in out Result);

private

   CRLF : constant String := ASCII.CR & ASCII.LF;
   procedure Put_Line (S : in String);
   procedure Put (S : in String);
   procedure Put (I : in Integer);
   procedure New_Line;

end AUnit.Reporter.XML2;
