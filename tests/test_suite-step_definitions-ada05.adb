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
with Ada.Directories;
with Util.IO;
with XReq;
with XReq.Steps;
with XReq.Steps.Handles;
with XReq.Step_Definitions;
with XReq.Step_Definition_List.Handles;
with XReq.Step_Definitions.Handles;
with XReq.Step_Definitions.Ada05;

use Ada.Strings.Unbounded;
use Ada.Directories;
use Util.IO;
use XReq;
use XReq.Steps;
use XReq.Steps.Handles;
use XReq.Step_Definition_List.Handles;
use XReq.Step_Definitions.Handles;
use XReq.Step_Definitions.Ada05;

package body Test_Suite.Step_Definitions.Ada05 is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite)
   is
   begin
      Ret.Add_Test (new Test_Sample1);
      Ret.Add_Test (new Test_Parse_Dir);
   end Add_Tests;

   --  Sample1  ---------------------------------------------------------------

   function  Name (T : in Test_Sample1) return String is
      pragma Unreferenced (T);
   begin
      return ("AsaSpec.Steps.Ada05 sample1.ads");
   end Name;

   procedure Run (T : in out Test_Sample1) is
      Step  : Ada_Step_File_Type;
      File  : constant String := "tests/features/step_definitions/sample1.ads";
      Given1 : constant String := "this step works";
      Given2 : constant String := "this step does not work";
   begin

      Make (Step, File);

      T.Assert (File_Name (Step) = File,
              "Step filename (" & File_Name (Step) & ") is incorrect");

      T.Assert (not Parsed (Step),
              "Step has been parsed without invoking Parse");

      declare
         Foo : Boolean;
         procedure P;
         procedure P is begin
            Foo := Contains (Step, Stanza_Given (Given1));
            T.Assert (Foo and not Foo, "Should never reach here");
         end P;
         procedure A is new Assert_Except (Test_Sample1, P);
      begin
         A (T, "Unparsed_Step has not been raised in call to Contains",
               XReq.Step_Definitions.Unparsed_Step'Identity);
      end;

      declare
         Match_V : Step_Match_Vectors.Vector;
         Proc_N  : Unbounded_String;
         Found   : Boolean;
         procedure P;
         procedure P is begin
            Find (Step, Stanza_Given (Given1), Proc_N, Match_V, Found);
            T.Assert (False, "Should never reach here");
         end P;
         procedure A is new Assert_Except (Test_Sample1, P);
      begin
         A (T, "Unparsed_Step has not been raised in call to Find",
            XReq.Step_Definitions.Unparsed_Step'Identity);
      end;

      Parse (Step, Std_Logger);

      T.Assert (Parsed (Step),
              "Step has not been parsed after invoking Parse");

      T.Assert (Contains (Step, Stanza_Given (Given1)),
              "Step should contain """ & Given1 & """");

      T.Assert (Contains (Step, Stanza_When ("this step works too")),
              "Step should contains `When this step works too'");

      T.Assert (not Contains (Step, Stanza_Given (Given2)),
              "Step should not contain """ & Given2 & """");

      T.Assert (Find (Step, Stanza_Given (Given1)) = "Sample1.This_Step_Works",
              "`Given " & Given1 & "' should find `Sample1.This_Step_Works'");

      T.Assert (Find (Step, Stanza_Given (Given2)) = "",
              "`Given " & Given2 & "' should find '");

   end Run;

   --  Parse_Directory  -------------------------------------------------------

   function  Name (T : in Test_Parse_Dir) return String is
      pragma Unreferenced (T);
   begin
      return ("AsaSpec.Steps.Ada05.Parse_Directory");
   end Name;

   procedure Run (T : in out Test_Parse_Dir) is
      Directory : constant String := "tests/features/step_definitions";
      Steps     : constant Step_File_List_Handle := Create;
      Step      : Ada_Step_File_Ptr;
      Found     : Boolean := False;
      I         : Natural;
   begin

      Parse_Directory (Steps, Std_Logger, Directory);

      T.Assert (Steps.R.Count >= 1,
              "Detected " & Steps.R.Count'Img & " steps instead of >= 1");

      I := Steps.R.First;
      while I < Steps.R.Last and not Found loop
         Step  := Ada_Step_File_Ptr (Steps.R.Element (I).R);
         Found := Simple_Name (File_Name (Step.all)) = "sample1.ads";
         Std_Logger.Put_Line ("Found step: " & File_Name (Step.all) & " (" &
                              Simple_Name (File_Name (Step.all)) & ")");
         I := I + 1;
      end loop;

      T.Assert (Found,
              "Should have detected step sample1.ads");

      T.Assert (Parsed (Step.all), "Should have parsed the step definition");

      T.Assert (Contains (Step.all, Stanza_Given ("this step works")),
              "The step definition should contain `Given this step works'");

   end Run;


end Test_Suite.Step_Definitions.Ada05;
