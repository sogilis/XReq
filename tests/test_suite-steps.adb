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

with XReqLib;
with XReq.Steps;
with XReq.Steps.Handles;

use XReqLib;
use XReq.Steps;
use XReq.Steps.Handles;

package body Test_Suite.Steps is

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
      S1, S2 : Step_Handle;
   begin

      T.Assert (Stanza_Given ("A").R.To_String = "Given A",
              "Wrong stanza Given A");

      T.Assert (Stanza_When  ("B").R.To_String = "When B",
              "Wrong stanza When B");

      T.Assert (Stanza_Then  ("C").R.To_String = "Then C",
              "Wrong stanza Then C");

      declare
         Expect : constant String :=
            "@given ^Something ""([^""]*)"" dumb \(""\)$";
         Found  : constant String :=
            Stanza_Given ("Something ""here"" dumb ("")").R.To_Regexp;
      begin
         T.Assert (Expect = Found, "To_Regexp not OK." & ASCII.LF &
                  "Expected: " & Expect & ASCII.LF &
                  "Found   : " & Found);
      end;

      S1 := Stanza_Given ("A");
      S2 := Stanza_Given ("A");

      S1.R.Set_Position (Position ("toto", 5));
      T.Assert (S1.R.Position = Position ("toto", 5), "Wrong position");

      S2.R.Set_Position (Position ("toto", 5));
      T.Assert (S1.R.all = S2.R.all, "Wrong Equals");

   end Run;

end Test_Suite.Steps;
