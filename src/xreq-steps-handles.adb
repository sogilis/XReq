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


package body XReq.Steps.Handles is

   use Handles_Pkg;

   -----------------------------------
   --  Step_Type  --  Stanza_Given  --
   -----------------------------------

   function Stanza_Given (S    : in String;
                          File : in String := "";
                          Line : in Natural := 0) return Step_Type is
   begin
      return Step : Step_Type do
         Step.Make (Step_Given, S, Position (File, Line));
      end return;
   end Stanza_Given;

   ----------------------------------
   --  Step_Type  --  Stanza_When  --
   ----------------------------------

   function Stanza_When  (S    : in String;
                          File : in String := "";
                          Line : in Natural := 0) return Step_Type is
   begin
      return Step : Step_Type do
         Step.Make (Step_When, S, Position (File, Line));
      end return;
   end Stanza_When;

   ----------------------------------
   --  Step_Type  --  Stanza_Then  --
   ----------------------------------

   function Stanza_Then  (S    : in String;
                          File : in String := "";
                          Line : in Natural := 0) return Step_Type is
   begin
      return Step : Step_Type do
         Step.Make (Step_Then, S, Position (File, Line));
      end return;
   end Stanza_Then;


end XReq.Steps.Handles;
