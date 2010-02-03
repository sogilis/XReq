--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO;
with GNAT.IO;
with AUnit.Test_Results;
with AUnit.Reporter;
with AUnit.Reporter.Text;
with AUnit.Reporter.XML;

--  ATTENTION: This package will never write the AUnit reports to a file
--  because the default reporters from AUnit do not use Ada.Text_IO but
--  GNAT.IO. Unfortunately, GNAT.IO do not offer a way to change the current
--  output. It can ONLY output to stdout or stderr (read g-io.adb).

package AUnit_Reporter is

   --  Static variables to avoid allocating memory dynamically.

   Reporter_Text : aliased AUnit.Reporter.Text.Text_Reporter;
   Reporter_XML  : aliased AUnit.Reporter.XML.XML_Reporter;
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

   overriding procedure Report (Engine : in     Reporter;
                                R      : in out AUnit.Test_Results.Result);

end AUnit_Reporter;
