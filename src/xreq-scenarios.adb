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

package body XReq.Scenarios is

   -------------------------------
   --  Scenario_Type  --  Make  --
   -------------------------------


   procedure Make         (Scenario : in out Scenario_Type;
                           Name     : in     String;
                           Position : in     Position_Type := Null_Position;
                           Outline  : in     Boolean := False;
                           Tags     : in     String_Vector :=
                             String_Vectors.Empty_Vector) is
   begin
      if Outline then
         Scenario := Scenario_Type'
           (Reffy.Counted_Type (Scenario) with
            D => (Outline => True,
                  Name    => To_Unbounded_String (Name),
                  Pos     => Position,
                  Tags    => Tags,
                  others  => <>));
      else
         Scenario := Scenario_Type'
           (Reffy.Counted_Type (Scenario) with
            D => (Outline => False,
                  Name    => To_Unbounded_String (Name),
                  Pos     => Position,
                  Tags    => Tags,
                  others  => <>));
      end if;
   end Make;

   ---------------------------------
   --  Scenario  --  Step_Append  --
   ---------------------------------

   procedure Step_Append (Scenario : in out Scenario_Type;
                     Stanza   : in     Step_Handle)
   is
      use Step_Vectors;
   begin
      Append (Scenario.D.Steps, Stanza);
   end Step_Append;

   --------------------------------
   --  Scenario  --  Step_First  --
   --------------------------------

   function  Step_First  (Scenario : in     Scenario_Type) return Natural is
      pragma Unreferenced (Scenario);
   begin
      return 0;
   end Step_First;

   -------------------------------
   --  Scenario  --  Step_Last  --
   -------------------------------

   function  Step_Last   (Scenario : in     Scenario_Type) return Integer is
      use Ada.Containers;
      use Step_Vectors;
   begin
      return Integer (Length (Scenario.D.Steps)) - 1;
   end Step_Last;

   --------------------------------
   --  Scenario  --  Step_Count  --
   --------------------------------

   function  Step_Count  (Scenario : in     Scenario_Type) return Natural is
      use Ada.Containers;
      use Step_Vectors;
   begin
      return Natural (Length (Scenario.D.Steps));
   end Step_Count;

   ----------------------------------
   --  Scenario  --  Step_Element  --
   ----------------------------------

   function  Step_Element (Scenario : in     Scenario_Type;
                           Index    : in     Natural)       return Step_Handle
   is
      use Step_Vectors;
   begin
      return Element (Scenario.D.Steps, Index);
   end Step_Element;

   -------------------------------
   --  Scenario  --  Tag_First  --
   -------------------------------

   function  Tag_First  (Scenario : in     Scenario_Type) return Natural is
      pragma Unreferenced (Scenario);
   begin
      return 0;
   end Tag_First;

   ------------------------------
   --  Scenario  --  Tag_Last  --
   ------------------------------

   function  Tag_Last   (Scenario : in     Scenario_Type) return Integer is
   begin
      return Tag_Count (Scenario) - 1;
   end Tag_Last;

   -------------------------------
   --  Scenario  --  Tag_Count  --
   -------------------------------

   function  Tag_Count  (Scenario : in     Scenario_Type) return Natural is
      use Ada.Containers;
      use String_Vectors;
   begin
      return Natural (Length (Scenario.D.Tags));
   end Tag_Count;

   ---------------------------------
   --  Scenario  --  Tag_Element  --
   ---------------------------------

   function  Tag_Element (Scenario : in     Scenario_Type;
                          Index    : in     Natural)       return String
   is
      use String_Vectors;
   begin
      return To_String (Element (Scenario.D.Tags, Index));
   end Tag_Element;

   -----------------------------
   --  Scenario  --  Outline  --
   -----------------------------

   function  Outline      (S : in Scenario_Type) return Boolean is
   begin
      return S.D.Outline;
   end Outline;

   --------------------------
   --  Scenario  --  Name  --
   --------------------------

   function  Name         (S : in Scenario_Type) return String is
   begin
      return To_String (S.D.Name);
   end Name;

   ------------------------------
   --  Scenario  --  Position  --
   ------------------------------

   function  Position     (S : in Scenario_Type) return Position_Type is
   begin
      return S.D.Pos;
   end Position;

   --------------------------------
   --  Scenario  --  Tag_Vector  --
   --------------------------------

   function  Tag_Vector   (S : in Scenario_Type) return String_Vector is
   begin
      return S.D.Tags;
   end Tag_Vector;

   ---------------------------
   --  Scenario  --  Table  --
   ---------------------------

   function  Table        (S : in Scenario_Type)
                           return XReqLib.String_Tables.Table is
   begin
      return S.D.Table;
   end Table;

   ----------------------------------
   --  Scenario_Type  -- Set_Name  --
   ----------------------------------

   procedure Set_Name     (S     : in out Scenario_Type;
                           Name  : in     String) is
   begin
      S.D.Name := To_Unbounded_String (Name);
   end Set_Name;

   -------------------------------
   --  Scenario  --  Set_Table  --
   -------------------------------

   procedure Set_Table    (S     : in out Scenario_Type;
                           Table : in XReqLib.String_Tables.Table) is
   begin
      S.D.Table := Table;
   end Set_Table;

   ----------------------------------
   --  Scenario  --  Output_Steps  --
   ----------------------------------

   procedure Output_Steps (S     : in     Scenario_Type;
                           Buf   : in out Unbounded_String) is
      Sta : Step_Handle;
      Pre : Step_All_Kind := Step_Null;
   begin
      for I in S.Step_First .. S.Step_Last loop
         Sta := S.Step_Element (I);
         Append (Buf, "    ");
         Append (Buf, Sta.R.To_String (Pre));
         Pre := Sta.R.Kind;
         Append (Buf, ASCII.LF);
      end loop;
   end Output_Steps;

   ---------------------------------
   --  Scenario_Type  --  Equals  --
   ---------------------------------

   function Equals (Left, Right : in Scenario_Type) return Boolean is
      L : constant access constant Scenario_Type'Class := Left'Access;
      R : constant access constant Scenario_Type'Class := Right'Access;
   begin
      return L.all = R.all;
   end Equals;

end XReq.Scenarios;
