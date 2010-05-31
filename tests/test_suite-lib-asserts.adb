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

with XReqLib.Asserts;

use XReqLib.Asserts;

package body Test_Suite.Lib.Asserts is

   procedure Add_Tests (Ret : in AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Ret.Add_Test (new Test_Assert);
   end Add_Tests;

   --  Test_Assert  -----------------------------------------------------------

   function  Name (T : in Test_Assert) return String is
      pragma Unreferenced (T);
   begin
      return "XReqLib.Asserts";
   end Name;

   procedure Run (T : in out Test_Assert) is
      package Lib renames XReqLib.Asserts;
   begin
      Lib.Assert (True, "This error shouldn't happen");
      begin
         Lib.Assert (False, "errmsg");
         T.Assert (False, "Assert should raise XReqLib.Asserts.Error");
      exception
         when E : Lib.Error =>
            T.Assert (Exception_Message (E) = "errmsg",
                      "Exception message not OK. Found: '" &
                      Exception_Message (E) & "'");
      end;
      Lib.Equals ("a", "a", "This error shouldn't happen");
      begin
         Lib.Equals ("a", "b", "errmsg");
         T.Assert (False, "Assert should raise XReqLib.Asserts.Error");
      exception
         when Lib.Error =>
            T.Assert (True, "");
      end;
      begin
         Lib.Equals ("a", "b");
         T.Assert (False, "Assert should raise XReqLib.Asserts.Error");
      exception
         when Lib.Error =>
            T.Assert (True, "");
      end;
   end Run;

end Test_Suite.Lib.Asserts;
