--                         Copyright (C) 2010, Sogilis                       --

with Util.Strings;

package body AdaSpec.Result_Steps is

   --------------------------------------
   --  Result_Step_Type  --  New_Step  --
   --------------------------------------

   function  New_Step    (Kind     : in Step_Kind;
                          Stanza   : in String;
                          Position : in Position_Type)
                          return Result_Step_Type is
   begin
      return Result_Step_Type'
        (AdaSpec.Steps.New_Step (Kind, Stanza, Position)
         with others => <>);
   end New_Step;

   -------------------------------------
   --  Stanza_Type  --  Stanza_Given  --
   -------------------------------------

   function Stanza_Given (S    : in String;
                          File : in String := "";
                          Line : in Natural := 0) return Result_Step_Type is
   begin
      return New_Step (Step_Given, S, Position (File, Line));
   end Stanza_Given;

   ------------------------------------
   --  Stanza_Type  --  Stanza_When  --
   ------------------------------------

   function Stanza_When  (S    : in String;
                          File : in String := "";
                          Line : in Natural := 0) return Result_Step_Type is
   begin
      return New_Step (Step_When, S, Position (File, Line));
   end Stanza_When;

   ------------------------------------
   --  Stanza_Type  --  Stanza_Then  --
   ------------------------------------

   function Stanza_Then  (S    : in String;
                          File : in String := "";
                          Line : in Natural := 0) return Result_Step_Type is
   begin
      return New_Step (Step_Then, S, Position (File, Line));
   end Stanza_Then;

   ---------------------------------------------
   --  Result_Step_Type  --  New_Result_Step  --
   ---------------------------------------------

   function  New_Result_Step (Step           : in  Step_Type;
                              Procedure_Name : in  String := "";
                              Matches        : in  Match_Vectors.Vector
                                             := Match_Vectors.Empty_Vector)
                                             return Result_Step_Type
   is
   begin
      return Result_Step_Type'
        (Step with
         Procedure_Name => To_Unbounded_String (Procedure_Name),
         Matches        => Matches);
   end New_Result_Step;


   --------------------------------------------
   --  Result_Step_Type  --  Procedure_Name  --
   --------------------------------------------

   function Procedure_Name (S : in Result_Step_Type) return String is
   begin
      return To_String (S.Procedure_Name);
   end Procedure_Name;

   ---------------------------------------
   --  Result_Step_Type  --  To_String  --
   ---------------------------------------

   function To_Code (S : in Result_Step_Type;
                       Indent : in String := "") return String
   is
      use Util.Strings;
      Buffer : Unbounded_String;
      E : Match_Location;
   begin
      Append (Buffer, Indent & Procedure_Name (S) & " (");
      for I in S.Match_First .. S.Match_Last loop
         E := S.Match_Element (I);
         Append (Buffer, String'("(" &
                 Ada_String (S.Stanza (E.First .. E.Last)) &
                 E.First'Img &
                 E.Last'Img & ")"));
      end loop;
      Append (Buffer, " );");
      return To_String (Buffer);
   end To_Code;

   ------------------------------------------
   --  Result_Step_Type  --  Process_Step  --
   ------------------------------------------

   procedure Process_Step     (Res      : out Result_Step_Type;
                               Stanza   : in  Step_Type;
                               Steps    : in  Step_Definitions_Type;
                               Log      : in  Logger_Ptr;
                               Errors   : out Boolean;
                               Step_Matching : in Boolean)
   is
      Match : Step_Match_Type;
   begin
      Errors := False;
      begin
         Match := Find (Steps, Stanza);
      exception
         when Ambiguous_Match =>
            Log.Put_Line (String'("Error: Ambiguous match in " &
                        To_String (Position (Stanza)) & " for:"));
            Log.Put_Line ("  " & To_String (Stanza));
            Errors := True;
      end;
      if not Match.Match then
         Log.Put_Line (String'("Error: Missing step definition in " &
                       To_String (Position (Stanza)) & " for:"));
         Log.Put_Line ("  " & To_String (Stanza));
         Log.Put_Line ("You can implement this step by adding on your " &
                       "step definition file:");
         Log.Put_Line ("  --  " & To_Regexp (Stanza));
         Log.Put_Line ("  --  @todo");
         Log.New_Line;
         Errors := True;
      elsif Step_Matching then
         Log.Put_Line ("Step Matching: """ & To_String (Position (Stanza)) &
                       """ matches """ & To_String (Match.Position) &
                       """ procedure " & To_String (Match.Proc_Name));
      end if;
      Res := New_Result_Step (Stanza);
      Res.Procedure_Name := Match.Proc_Name;
      Res.Matches        := Match.Matches;
   end Process_Step;

   --------------------------
   --  Set_Procedure_Name  --
   --------------------------

   procedure Set_Procedure_Name (S   : in out Result_Step_Type;
                                 Prc : in     String) is
   begin
      S.Procedure_Name := To_Unbounded_String (Prc);
   end Set_Procedure_Name;

   ----------------------------------
   --  Step_Type  --  Match_First  --
   ----------------------------------

   function Match_First   (S : in Result_Step_Type) return Natural is
      pragma Unreferenced (S);
   begin
      return 0;
   end Match_First;

   ---------------------------------
   --  Step_Type  --  Match_Last  --
   ---------------------------------

   function Match_Last    (S : in Result_Step_Type) return Integer is
   begin
      return Match_Count (S) - 1;
   end Match_Last;

   ----------------------------------
   --  Step_Type  --  Match_Count  --
   ----------------------------------

   function Match_Count   (S : in Result_Step_Type) return Natural is
      use Match_Vectors;
   begin
      return Integer (Length (S.Matches));
   end Match_Count;

   ------------------------------------
   --  Step_Type  --  Match_Element  --
   ------------------------------------

   function Match_Element (S : in Result_Step_Type;
                           I : in Natural)          return Match_Location
   is
      use Match_Vectors;
   begin
      return Element (S.Matches, I);
   end Match_Element;

   -----------------------------
   --  Step_Type  --  Equals  --
   -----------------------------

   function Equals (Left, Right : in Result_Step_Type) return Boolean is
      L : constant access constant Result_Step_Type'Class := Left'Access;
      R : constant access constant Result_Step_Type'Class := Right'Access;
   begin
      return L.all = R.all;
   end Equals;

end AdaSpec.Result_Steps;
