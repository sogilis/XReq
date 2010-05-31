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

with Ada.Unchecked_Deallocation;
with Ada.Strings;
with Ada.Strings.Fixed;

package body XReqLib.Generic_Features is

   ------------------------------
   --  Feature_Type  --  Make  --
   ------------------------------

   procedure Make (F      : out    Feature_Type;
                   Name   : in     String := "")
   is
      Feature : Feature_Type := Null_Feature;
   begin
      Feature.Name := To_Unbounded_String (Name);
      F := Feature;
   end Make;

   --------------------------------
   --  Feature_Type  --  Parsed  --
   --------------------------------

   function Parsed (F : in Feature_Type) return Boolean is
      pragma Unreferenced (F);
   begin
      return True;
   end Parsed;

   ------------------------------
   --  Feature_Type  --  Name  --
   ------------------------------

   function Name (F : in Feature_Type) return String is
   begin
      return To_String (F.Name);
   end Name;

   ----------------------------------
   --  Feature_Type  --  Position  --
   ----------------------------------

   function Position (F : in Feature_Type) return Position_Type is
   begin
      return F.Pos;
   end Position;

   -------------------------------------
   --  Feature_Type  --  Description  --
   -------------------------------------

   function Description (F : in Feature_Type) return String is
   begin
      return To_String (F.Description);
   end Description;

   ------------------------------------
   --  Feature_Type  --  Background  --
   ------------------------------------

   function  Background  (F : in Feature_Type) return Scenario_Type is
   begin
      return F.Background;
   end Background;

   ----------------------------------
   --  Feature_Type  --  Filetype  --
   ----------------------------------

   function  Filetype    (F : in Feature_Type) return String is
   begin
      return To_String (F.Filetype);
   end Filetype;

   -----------------------------------
   --  Feature_Type  --  To_String  --
   -----------------------------------

   function To_String (F : in Feature_Type) return String is

      Self : constant access constant Feature_Type'Class := F'Access;

      use Scenario_Container;
      CRLF : constant String := "" & ASCII.LF;
      Res  : Unbounded_String;
      Cur  : Scenario_Container.Cursor := First (F.Scenarios);
      Sce  : Scenario_Type;

   begin
      if not Parsed (Self.all) then
         raise Unparsed_Feature;
      end if;
      Append (Res, "Feature: " & To_String (F.Name) & CRLF);
      Append (Res, CRLF);
      Append (Res, "  Background: " & F.Background.Name & CRLF);
      F.Background.Output_Steps (Res);
      Append (Res, CRLF);
      while Has_Element (Cur) loop
         Sce := Element (Cur);
         Append (Res, "  Scenario: " & Sce.Name & CRLF);
         Sce.Output_Steps (Res);
         Append (Res, CRLF);
         Next (Cur);
      end loop;
      return To_String (Res);
   end To_String;

   ----------------------------------------
   --  Feature_Type  --  Set_Background  --
   ----------------------------------------

   procedure Set_Name           (F      : in out Feature_Type;
                                 Name   : in     String) is
   begin
      F.Name := To_Unbounded_String (Name);
   end Set_Name;

   ----------------------------------------
   --  Feature_Type  --  Set_Background  --
   ----------------------------------------

   procedure Set_Position       (F      : in out Feature_Type;
                                 Pos    : in     Position_Type) is
   begin
      F.Pos := Pos;
   end Set_Position;

   ----------------------------------------
   --  Feature_Type  --  Set_Background  --
   ----------------------------------------

   procedure Set_Background (F      : in out Feature_Type;
                             Bg     : in     Scenario_Type)
   is
   begin
      F.Background := Bg;
   end Set_Background;

   -----------------------------------------
   --  Feature_Type  --  Set_Description  --
   -----------------------------------------

   procedure Set_Description (F      : in out Feature_Type;
                              Desc   : in     String)
   is
   begin
      F.Description := To_Unbounded_String (Desc);
   end Set_Description;

   --------------------------------------
   --  Feature_Type  --  Set_Filetype  --
   --------------------------------------

   procedure Set_Filetype       (F      : in out Feature_Type;
                                 FType  : in     String) is
   begin
      F.Filetype := To_Unbounded_String (FType);
   end Set_Filetype;

   --------------------------------------------
   --  Feature_Type  --  Append_Description  --
   --------------------------------------------

   procedure Append_Description (F      : in out Feature_Type;
                                 Desc   : in     String)
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      D : constant String := Trim (Description (F), Right);
   begin
      if D /= "" then
         F.Set_Description (D & ASCII.LF & Desc);
      else
         F.Set_Description (Desc);
      end if;
   end Append_Description;

   ----------------------------------------
   --  Feature_Type  --  Scenario_First  --
   ----------------------------------------

   function  Scenario_First     (F : in Feature_Type) return Natural
   is
      pragma Unreferenced (F);
   begin
      return 0;
   end Scenario_First;

   ---------------------------------------
   --  Feature_Type  --  Scenario_Last  --
   ---------------------------------------

   function  Scenario_Last      (F : in Feature_Type) return Integer
   is
   begin
      return Scenario_Count (F) - 1;
   end Scenario_Last;

   ----------------------------------------
   --  Feature_Type  --  Scenario_Count  --
   ----------------------------------------

   function  Scenario_Count     (F : in Feature_Type) return Natural
   is
      use Scenario_Container;
   begin
      return Natural (Length (F.Scenarios));
   end Scenario_Count;

   ---------------------------------
   --  Feature_Type  --  Element  --
   ---------------------------------

   function  Scenario_Element   (F : in Feature_Type;
                                 I : in Natural)      return Scenario_Type
   is
      use Scenario_Container;
   begin
      return Element (F.Scenarios, I);
   end Scenario_Element;

   --------------------------------
   --  Feature_Type  --  Append  --
   --------------------------------

   procedure Scenario_Append    (F : in out Feature_Type;
                                 S : in     Scenario_Type)
   is
      use Scenario_Container;
   begin
      Append (F.Scenarios, S);
   end Scenario_Append;

   ------------
   --  Free  --
   ------------

   procedure Free (F : in out Feature_Ptr) is
      procedure DeAlloc is new Ada.Unchecked_Deallocation
         (Feature_Type'Class, Feature_Ptr);
   begin
      DeAlloc (F);
   end Free;

end XReqLib.Generic_Features;
