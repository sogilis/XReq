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

package body XReqLib.Asserts is

   --------------
   --  Assert  --
   --------------

   procedure Assert (Cmp : in Boolean; Reason : in String := "") is
   begin
      if not Cmp then
         if Reason /= "" then
            raise Error with Reason;
         else
            raise Error with "Assertion failed";
         end if;
      end if;
   end Assert;

   --------------
   --  Equals  --
   --------------

   procedure Equals (T1, T2 : in String; Reason : in String := "") is
   begin
      if T1 /= T2 then
         if Reason /= "" then
            raise Error with Reason & ASCII.LF &
               "Expected two strings to be equals:" & ASCII.LF &
               T1 & ASCII.LF & "--  is not the same as  --" & ASCII.LF &
               T2 & ASCII.LF & "--";
         else
            raise Error with
               "Expected two strings to be equals:" & ASCII.LF &
               T1 & ASCII.LF & "--  is not the same as  --" & ASCII.LF &
               T2 & ASCII.LF & "--";
         end if;
      end if;
   end Equals;

end XReqLib.Asserts;
