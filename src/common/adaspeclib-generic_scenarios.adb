--                         Copyright (C) 2010, Sogilis                       --

package body AdaSpecLib.Generic_Scenarios is

   ----------------------------------
   --  Scenario  --  New_Scenario  --
   ----------------------------------

   function  New_Scenario (Name     : in     String;
                           Position : in     Position_Type := Null_Position;
                           Outline  : in     Boolean := False;
                           Tags     : in     String_Vector :=
                                             String_Vectors.Empty_Vector)
                                      return Scenario_Type
   is
   begin
      if Outline then
         return Scenario_Type'(D => (Outline => True,
                                     Name    => To_Unbounded_String (Name),
                                     Pos     => Position,
                                     Tags    => Tags,
                                     others  => <>));
      else
         return Scenario_Type'(D => (Outline => False,
                                     Name    => To_Unbounded_String (Name),
                                     Pos     => Position,
                                     Tags    => Tags,
                                     others  => <>));
      end if;
   end New_Scenario;

   ----------------------------
   --  Scenario  --  Append  --
   ----------------------------

   procedure Step_Append (Scenario : in out Scenario_Type;
                     Stanza   : in     Step_Type)
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
                           Index    : in     Natural)       return Step_Type
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
                           return AdaSpecLib.String_Tables.Table is
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
                           Table : in AdaSpecLib.String_Tables.Table) is
   begin
      S.D.Table := Table;
   end Set_Table;

   ----------------------------------
   --  Scenario  --  Output_Steps  --
   ----------------------------------

   procedure Output_Steps (S     : in     Scenario_Type;
                           Buf   : in out Unbounded_String) is
      Sta : Step_Type;
      Pre : Step_All_Kind := Step_Null;
   begin
      for I in S.Step_First .. S.Step_Last loop
         Sta := S.Step_Element (I);
         Append (Buf, "    ");
         Append (Buf, Sta.To_String (Pre));
         Pre := Sta.Kind;
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

end AdaSpecLib.Generic_Scenarios;
