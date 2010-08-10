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

package body XReq.Language is

   procedure Set_Type (L : in out Language_Type; Typ : in String) is
   begin
      if Typ = "feature" then
         L.Typ := Feature;
      elsif Typ = "requirement" then
         L.Typ := Requirement;
      else
         raise Unknown_Type with "Unknown file type " & Typ;
      end if;
   end Set_Type;

   function Feature          (L : in Language_Type) return String is
   begin
      case L.Typ is
         when Feature =>        return "Feature:";
         when Requirement =>    return "Requirement:";
      end case;
   end Feature;

   function Background       (L : in Language_Type) return String is
      pragma Unreferenced (L);
   begin
      return "Background:";
   end Background;

   function Scenario         (L : in Language_Type) return String is
   begin
      case L.Typ is
         when Feature =>        return "Scenario:";
         when Requirement =>    return "Test Case:";
      end case;
   end Scenario;

   function Scenario_Outline (L : in Language_Type) return String is
   begin
      case L.Typ is
         when Feature =>        return "Scenario Outline:";
         when Requirement =>    return "Test Case Template:";
      end case;
   end Scenario_Outline;

   function Examples         (L : in Language_Type) return String is
      pragma Unreferenced (L);
   begin
      return "Examples:";
   end Examples;

   function Given            (L : in Language_Type) return String is
      pragma Unreferenced (L);
   begin
      return "Given ";
   end Given;

   function When_K           (L : in Language_Type) return String is
      pragma Unreferenced (L);
   begin
      return "When ";
   end When_K;

   function Then_K           (L : in Language_Type) return String is
      pragma Unreferenced (L);
   begin
      return "Then ";
   end Then_K;

   function And_K            (L : in Language_Type) return String is
      pragma Unreferenced (L);
   begin
      return "And ";
   end And_K;

   function StrSimple        (L : in Language_Type) return String is
      pragma Unreferenced (L);
   begin
      return """""""";
   end StrSimple;

   function StrDouble        (L : in Language_Type) return String is
      pragma Unreferenced (L);
   begin
      return "'''";
   end StrDouble;

end XReq.Language;
