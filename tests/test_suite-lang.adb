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

with XReq.Lang;

use XReq.Lang;

package body Test_Suite.Lang is

   procedure Add_Tests (Ret : in AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Ret.Add_Test (new Test_1);
   end Add_Tests;

   --  Test_1  ----------------------------------------------------------------

   function  Name (T : in Test_1) return String is
      pragma Unreferenced (T);
   begin
      return "XReq.Lang";
   end Name;

   procedure Run (T : in out Test_1) is
   begin

      T.Assert (Get_Language ("aDa") = Lang_Ada,
                "Could not detect language aDa");

      declare
         Lang : Language_Type;
         pragma Unreferenced (Lang);
         procedure P;
         procedure P is begin
            Lang := Get_Language ("No_Language");
         end P;
         procedure A is new Assert_Except (Test_1, P);
      begin
         A (T, "Get_Language (No_Language) should raise Invalid_Language",
            Invalid_Language'Identity);
      end;

   end Run;

end Test_Suite.Lang;
