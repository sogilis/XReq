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

with Reffy.Handles;

package XReq.Steps.Handles is

   package Handles_Pkg is new Reffy.Handles
     (Step_Type, Step_Ptr);

   subtype Step_Handle is Handles_Pkg.Handle;

   function  Stanza_Given (S    : in String;
                           File : in String := "";
                           Line : in Natural := 0) return Step_Type;
   function  Stanza_When  (S    : in String;
                           File : in String := "";
                           Line : in Natural := 0) return Step_Type;
   function  Stanza_Then  (S    : in String;
                           File : in String := "";
                           Line : in Natural := 0) return Step_Type;

end XReq.Steps.Handles;


