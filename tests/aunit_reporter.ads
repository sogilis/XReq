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

with Ada.Text_IO;
with GNAT.IO;
with AUnit.Test_Results;
with AUnit.Reporter;
with AUnit.Reporter.Text2;
with AUnit.Reporter.XML2;

--  ATTENTION: This package will never write the AUnit reports to a file
--  because the default reporters from AUnit do not use Ada.Text_IO but
--  GNAT.IO. Unfortunately, GNAT.IO do not offer a way to change the current
--  output. It can ONLY output to stdout or stderr (read g-io.adb).

package AUnit_Reporter is

   --  Static variables to avoid allocating memory dynamically.

   Reporter_Text : aliased AUnit.Reporter.Text2.Text_Reporter;
   Reporter_XML  : aliased AUnit.Reporter.XML2.XML_Reporter;
   Output_File   : aliased Ada.Text_IO.File_Type;
   GNAT_IO_out   : aliased GNAT.IO.File_Type := GNAT.IO.Standard_Output;
   GNAT_IO_err   : aliased GNAT.IO.File_Type := GNAT.IO.Standard_Error;

   type Reporter is new AUnit.Reporter.Reporter with
      record
         Reporter : access AUnit.Reporter.Reporter'Class
                  := Reporter_Text'Access;
         File     : Ada.Text_IO.File_Access  := Ada.Text_IO.Current_Output;
         GNAT_IO  : access GNAT.IO.File_Type := GNAT_IO_out'Access;
      end record;

   overriding procedure Report (Engine : in Reporter;
                                R  : in out AUnit.Test_Results.Result'Class);

end AUnit_Reporter;
