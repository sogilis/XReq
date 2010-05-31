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
with Ada.Containers;
with Util.IO;
with XReq.Lang;
with XReq.Step_Definitions;
with XReq.Steps;

use Ada.Strings.Unbounded;
use Ada.Containers;
use Util.IO;
use XReq.Lang;
use XReq.Step_Definitions;
use XReq.Steps;

package body Test_Suite.Step_Definitions is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite)
   is
   begin
      Ret.Add_Test (new Test_1);
   end Add_Tests;

   --  Test_1  ----------------------------------------------------------------

   function  Name (T : in Test_1) return String is
      pragma Unreferenced (T);
   begin
      return ("XReq.Steps");
   end Name;

   procedure Run (T : in out Test_1) is
      use Match_Vectors;
      Steps   : Step_Definitions_Type;
      Dir     : constant String := "tests/features/step_definitions";
      Match_V : Match_Vectors.Vector;
      Proc_N  : Unbounded_String;
      StanzaS : constant String := "I match ""abc""";
      Stanza  : constant Step_Type := Stanza_When (StanzaS);
      Found   : Boolean;
      Loc     : Match_Location;
   begin

      Load (Steps, Std_Logger, Dir, Lang_Ada);

      T.Assert (Contains (Steps, Stanza_Given ("this step works")),
              Dir & " should contains `Given this step works'");

      T.Assert (Contains (Steps, Stanza_When ("this step works too")),
              Dir & " should contains `When this step works too'");

      T.Assert (Find (Steps, Stanza_When ("this step works too")) =
              "Sample1.This_Step_Works_Too",
              "`When this step works too' and link " &
              "to procedure Sample1.This_Step_Works_Too");

      T.Assert (not Contains (Steps, Stanza_Then ("this step doesn't works")),
              Dir & " should not contains `Then this step doesn't works'");

      Find (Steps, Stanza_When ("I match nothing"), Proc_N, Match_V, Found);
      T.Assert (not Found, "Found");

      Find (Steps, Stanza, Proc_N, Match_V, Found);

      T.Assert (Found, "Not found");
      T.Assert (To_String (Proc_N) = "Sample2.When_I_Match",
              "Find should find Sample2.When_I_Match");
      T.Assert (Length (Match_V) = 1,
              "Find should get two captures");

--       Loc := Element (Match_V, 0);
--       T.Assert (Loc.First = StanzaS'First,
--               "Find: match 0 should start at" & StanzaS'First'Img &
--               " instead of" & Loc.First'Img);
--       T.Assert (Loc.Last = StanzaS'Last,
--               "Find: match 0 should end at" & StanzaS'Last'Img &
--               " instead of" & Loc.Last'Img);
--       Loc := Element (Match_V, 1);

      Loc := Element (Match_V, 0);
      T.Assert (Loc.First = 10,
              "Find: match 1 should start at 10" &
              " instead of" & Loc.First'Img);
      T.Assert (Loc.Last = 12,
              "Find: match 1 should end at 12" &
              " instead of" & Loc.Last'Img);

      Free (Steps);

   end Run;

end Test_Suite.Step_Definitions;
