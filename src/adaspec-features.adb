--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO;
with Util.IO;

package body AdaSpec.Features is

   -------------------------------------
   --  Stanza_Type  --  Stanza_Given  --
   -------------------------------------

   function Stanza_Given (S : in String) return Stanza_Type is begin
      return Stanza_Type'(Prefix => Prefix_Given,
                          Stanza => To_Unbounded_String (S));
   end Stanza_Given;

   ------------------------------------
   --  Stanza_Type  --  Stanza_When  --
   ------------------------------------

   function Stanza_When  (S : in String) return Stanza_Type is begin
      return Stanza_Type'(Prefix => Prefix_When,
                          Stanza => To_Unbounded_String (S));
   end Stanza_When;

   ------------------------------------
   --  Stanza_Type  --  Stanza_Then  --
   ------------------------------------

   function Stanza_Then  (S : in String) return Stanza_Type is begin
      return Stanza_Type'(Prefix => Prefix_Then,
                          Stanza => To_Unbounded_String (S));
   end Stanza_Then;

   --------------------------
   --  Scenario  --  Make  --
   --------------------------

   procedure Make   (Scenario : out    Scenario_Type;
                     Name     : in     String := "")
   is
      S : Scenario_Type := Null_Scenario;
   begin
      S.Name   := To_Unbounded_String (Name);
      Scenario := S;
   end Make;

   ----------------------------
   --  Scenario  --  Append  --
   ----------------------------

   procedure Append (Scenario : in out Scenario_Type;
                     Stanza   : in     Stanza_Type)
   is
      use Stanza_Container;
   begin
      Append (Scenario.Stanzas, Stanza);
   end Append;

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

   -------------------------------------
   --  Feature_File_Type  --  Parsed  --
   -------------------------------------

   function Parsed (F : in Feature_File_Type) return Boolean is
   begin
      return F.Parsed;
   end Parsed;

   ------------------------------
   --  Feature_Type  --  Name  --
   ------------------------------

   function Name (F : in Feature_Type) return String is
   begin
      return To_String (F.Name);
   end Name;

   ------------------------------------------------
   --  Feature_File_Type  --  Null_Feature_File  --
   ------------------------------------------------

   function Null_Feature_File return Feature_File_Type is
      F : Feature_File_Type;
   begin
      return F;
   end Null_Feature_File;

   -----------------------------------
   --  Feature_File_Type  --  Make  --
   -----------------------------------

   procedure Make (F         : out Feature_File_Type;
                   File_Name : in  String)
   is
      Feature : Feature_File_Type := Null_Feature_File;
   begin
      Feature.File_Name := To_Unbounded_String (File_Name);
      Feature.Parsed    := False;
      F := Feature;
   end Make;

   ------------------------------------
   --  Feature_File_Type  --  Parse  --
   ------------------------------------

   procedure Parse (F : in out Feature_File_Type) is

      Self : constant access Feature_File_Type'Class := F'Access;

      use Ada.Text_IO;
      use Util.IO;
      use Util.Strings;
      use Scenario_Container;
      use Stanza_Container;

      type Mode_Type   is (M_Begin, M_Feature, M_Background, M_Scenario);

      K_Feature    : constant String := "Feature:";
      K_Background : constant String := "Background:";
      K_Scenario   : constant String := "Scenario:";
      K_Given      : constant String := "Given ";
      K_When       : constant String := "When ";
      K_Then       : constant String := "Then ";
      K_And        : constant String := "And ";
      State        : Mode_Type         := M_Begin;
      Stanza_State : Prefix_Type_Maybe := Prefix_None;
      File         : File_Type;
      Line_S       : Unbounded_String;
      Suffix       : Unbounded_String;
      Idx_Start    : Natural;

      Current_Scenario : Scenario_Type;
      Current_Stanza   : Stanza_Type;

      function Starts_With_K (Keyword : in String) return Boolean;
      function Starts_With_K (Keyword : in String) return Boolean is
      begin
         return Starts_With (To_String (Line_S), Keyword, Idx_Start);
      end Starts_With_K;

      procedure Add_Scenario;
      procedure Add_Stanza;

      procedure Add_Scenario is
      begin
         Add_Stanza;
         if State = M_Scenario then
            Append (Self.Scenarios, Current_Scenario);
         elsif State = M_Background then
            Self.Background.Stanzas := Current_Scenario.Stanzas;
         end if;
         Current_Scenario.Name := Null_Unbounded_String;
         Clear (Current_Scenario.Stanzas);
      end Add_Scenario;

      procedure Add_Stanza is
      begin
         if Stanza_State /= Prefix_None then
            Append (Current_Scenario.Stanzas, Current_Stanza);
            Current_Stanza.Stanza := Null_Unbounded_String;
         end if;
      end Add_Stanza;

   begin
      Open (File, In_File, To_String (Self.File_Name));
      while not End_Of_File (File) loop
         --
         --  Read Line
         --
         Line_S    := Get_Whole_Line (File);
         Idx_Start := Index_Non_Blank (Line_S);

--          Put_Line ("READ: " & To_String (Line_S));
         --
         --  State Machine
         --
         case State is
            when M_Begin =>

               --  Found: "Feature: Name"
               if Starts_With_K (K_Feature) then
                  State := M_Feature;
                  Stanza_State := Prefix_None;
                  Self.Name := Trimed_Suffix (Line_S,
                                           Idx_Start + K_Feature'Length);
               end if;

            when M_Feature =>

               --  Found "Background:"
               if Starts_With_K (K_Background) then
                  Add_Scenario;
                  State := M_Background;
                  Stanza_State := Prefix_None;
                  Self.Background.Name :=
                     Trimed_Suffix (Line_S, Idx_Start + K_Background'Length);

               --  Found "Scenario: Name"
               elsif Starts_With_K (K_Scenario) then
                  Add_Scenario;
                  State := M_Scenario;
                  Stanza_State := Prefix_None;
                  Current_Scenario.Name :=
                     Trimed_Suffix (Line_S, Idx_Start + K_Scenario'Length);
               end if;

            when M_Background | M_Scenario =>

               --  Found "Background:"
               if Starts_With_K (K_Background) then
                  Add_Scenario;
                  State := M_Background;
                  Stanza_State := Prefix_None;
                  Self.Background.Name :=
                     Trimed_Suffix (Line_S, Idx_Start + K_Background'Length);

               --  Found "Scenario: Name"
               elsif Starts_With_K (K_Scenario) then
                  Add_Scenario;
                  State := M_Scenario;
                  Stanza_State := Prefix_None;
                  Current_Scenario.Name :=
                     Trimed_Suffix (Line_S, Idx_Start + K_Scenario'Length);

               --  Found "Given ..."
               elsif Starts_With_K (K_Given) then
                  Add_Stanza;
                  Stanza_State := Prefix_Given;
                  Idx_Start := Idx_Start + K_Given'Length;

               --  Found "When ..."
               elsif Starts_With_K (K_When) then
                  Add_Stanza;
                  Stanza_State := Prefix_When;
                  Idx_Start := Idx_Start + K_When'Length;

               --  Found "Then ..."
               elsif Starts_With_K (K_Then) then
                  Add_Stanza;
                  Stanza_State := Prefix_Then;
                  Idx_Start := Idx_Start + K_Then'Length;

               --  Found "And ..."
               elsif Starts_With_K (K_And) then
                  Add_Stanza;
                  Idx_Start := Idx_Start + K_And'Length;
               end if;

               --  Record stanza
               if Stanza_State /= Prefix_None then
                  Current_Stanza.Prefix := Stanza_State;
                  Suffix := Trimed_Suffix (Line_S, Idx_Start);
                  if Current_Stanza.Stanza /= Null_Unbounded_String and
                     Suffix                /= Null_Unbounded_String
                  then
                     Append (Current_Stanza.Stanza, " ");
                  end if;
                  Append (Current_Stanza.Stanza, Suffix);
               end if;
         end case;
--          Put_Line ("STATE: " & State'Img & " " & Stanza_State'Img);
      end loop;
      Add_Scenario;
      Close (File);
      Self.Parsed := True;
   end Parse;

   ----------------------------------------
   --  Feature_File_Type  --  File_Name  --
   ----------------------------------------

   function  File_Name (F : in Feature_File_Type) return String is
   begin
      return To_String (F.File_Name);
   end File_Name;


   ----------------------------------------
   --  Feature_File_Type  --  To_String  --
   ----------------------------------------

   function To_String (F : in Feature_File_Type) return String is
      CRLF : constant String := ASCII.CR & ASCII.LF;
   begin
      return "# File: " & To_String (F.File_Name) & CRLF &
             To_String (Feature_Type (F));
   end To_String;


   -----------------------------------
   --  Feature_Type  --  To_String  --
   -----------------------------------

   function To_String (F : in Feature_Type) return String is

      Self : constant access constant Feature_Type'Class := F'Access;

      use Scenario_Container;
      CRLF : constant String := ASCII.CR & ASCII.LF;
      Res  : Unbounded_String;
      Cur  : Scenario_Container.Cursor := First (F.Scenarios);
      Sce  : Scenario_Type;

      procedure Output_Stanzas (V : in Stanza_Container.Vector);
      procedure Output_Stanzas (V : in Stanza_Container.Vector) is
         use Stanza_Container;
         Pre  : Prefix_Type_Maybe := Prefix_None;
         Cur  : Stanza_Container.Cursor := First (V);
         Sta  : Stanza_Type;
      begin
         while Has_Element (Cur) loop
            Append (Res, "    ");
            Sta := Element (Cur);
            if Sta.Prefix = Pre then
               Append (Res, "And");
            else
               case Sta.Prefix is
                  when Prefix_Given => Append (Res, "Given");
                  when Prefix_When =>  Append (Res, "When");
                  when Prefix_Then =>  Append (Res, "Then");
               end case;
               Pre := Sta.Prefix;
            end if;
            Append (Res, " ");
            Append (Res, To_String (Sta.Stanza));
            Append (Res, CRLF);
            Next (Cur);
         end loop;
      end Output_Stanzas;

   begin
      if not Parsed (Self.all) then
         raise Unparsed_Feature;
      end if;
      Append (Res, "Feature: " & To_String (F.Name) & CRLF);
      Append (Res, CRLF);
      Append (Res, "  Background: " & To_String (F.Background.Name) & CRLF);
      Output_Stanzas (F.Background.Stanzas);
      Append (Res, CRLF);
      while Has_Element (Cur) loop
         Sce := Element (Cur);
         Append (Res, "  Scenario: " & To_String (Sce.Name) & CRLF);
         Output_Stanzas (Sce.Stanzas);
         Append (Res, CRLF);
         Next (Cur);
      end loop;
      return To_String (Res);
   end To_String;

   ------------------------------
   --  Feature_Type  --  Same  --
   ------------------------------

   function Same (F1, F2 : in Feature_Type'Class) return Boolean is
      use Util.Strings.Vectors;
      use Scenario_Container;
   begin
      return
         F1.Name        = F2.Name and
         F1.Description = F2.Description and
         F1.Background  = F2.Background and
         F1.Scenarios   = F2.Scenarios;
   end Same;

   --------------------------------
   --  Feature_Type  --  Append  --
   --------------------------------

   procedure Append         (F      : in out Feature_Type;
                             S      : in     Scenario_Type)
   is
      use Scenario_Container;
   begin
      Append (F.Scenarios, S);
   end Append;

   ----------------------------------------
   --  Feature_Type  --  Set_Background  --
   ----------------------------------------

   procedure Set_Background (F      : in out Feature_Type;
                             Bg     : in     Scenario_Type)
   is
   begin
      F.Background := Bg;
   end Set_Background;

end AdaSpec.Features;
