--                         Copyright (C) 2010, Sogilis                       --

package body AdaSpec.Result_Steps is

   --------------------------------------
   --  Result_Step_Type  --  New_Step  --
   --------------------------------------

   function  New_Step    (Kind     : in Step_Kind;
                          Stanza   : in String;
                          Position : in Position_Type)
                          return Result_Step_Type is
   begin
      return Step_Type'(Prefix => Kind,
                        Stanza => To_Unbounded_String (Stanza),
                        Pos    => Position,
                        others => <>);
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

   function  New_Result_Step (Procedure_Name : in  String;
                              Step           : in  Step_Type;
                              Matches        : in  Match_Vectors.Vector
                                             := Match_Vectors.Empty_Vector)
                                             return Result_Step_Type
   is
   begin
      return Result_Step_Type'(
         Procedure_Name => To_Unbounded_String (Procedure_Name),
         Step           => Step,
         Matches        => Matches)
   end Create;


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

   function To_String (S : in Result_Step_Type;
                       Indent : in String := "") return String
   is
      use Match_Vectors;
      Buffer : Unbounded_String;
      I : Cursor := First (S.Matches);
   begin
      Append (Buffer, Indent & Procedure_Name (S) & " (");
      while Has_Element (I) loop
         Append (Buffer, "(" &
                 Ada_String (S.Step.Stanza
                    (Element (I).First .. Element (I).Last)) &
                 Element (I).First'Img &
                 Element (I).Last'Img & ")");
         Next (I);
      end loop;
      Append (Buffer, " );");
      return To_String (Buffer);
   end To_String;

   ------------------------------------------
   --  Result_Step_Type  --  Process_Step  --
   ------------------------------------------

   procedure Process_Step     (Res      : out Result_Step_Type;
                               Stanza   : in  Step_Type;
                               Steps    : in  Step_Definitions_Type;
                               Log      : in  Logger_Ptr;
                               Errors   : out Boolean;
                               Step_Matching : in Boolean);
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
      Make (Res, To_String (Match.Proc_Name), Stanza, Match.Matches);
   end Process_Step;

end AdaSpec.Result_Steps;
