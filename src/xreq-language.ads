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

with Reffy;

package XReq.Language is

   type Language_Type is new Reffy.Counted_Type with private;
   type Language_Ptr is access all Language_Type;

   procedure Set_Type (L : in out Language_Type; Typ : in String);
   Unknown_Type : exception;

   function Feature          (L : in Language_Type) return String;
   function Background       (L : in Language_Type) return String;
   function Scenario         (L : in Language_Type) return String;
   function Scenario_Outline (L : in Language_Type) return String;
   function Examples         (L : in Language_Type) return String;
   function Given            (L : in Language_Type) return String;
   function When_K           (L : in Language_Type) return String;
   function Then_K           (L : in Language_Type) return String;
   function And_K            (L : in Language_Type) return String;
   function StrSimple        (L : in Language_Type) return String;
   function StrDouble        (L : in Language_Type) return String;

private

   type Type_Type is (Feature, Requirement);
   type Language_Type is
      new Reffy.Counted_Type with record
         Typ : Type_Type := Feature;
      end record;

end XReq.Language;
