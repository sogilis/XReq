--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;

use Ada.Strings.Unbounded;

package body AdaSpec.Result_Features is

   ------------------------------------------------
   --  Result_Feature_Type  --  Process_Feature  --
   ------------------------------------------------

   procedure Process_Feature (Res     : out Result_Feature_Type;
                              Feature : in  Feature_Ptr;
                              Steps   : in  Step_Definitions_Type;
                              Log     : in  Logger_Ptr;
                              Missing_Steps : in out String_Set;
                              Step_Matching : in     Boolean := False)
   is
      R_Scen : Result_Scenario_Type;
      Result : Result_Feature_Type;
      Errors : Boolean;
   begin
      if not Feature.Parsed then
         raise Unparsed_Feature;
      end if;
      Make (Result, Feature.Name);
      Result.Set_Position    (Feature.Position);
      Result.Set_Description (Feature.Description);
      Process_Scenario (R_Scen, Feature.Background,
                        Steps,
                        Log, Errors, Missing_Steps, Step_Matching);
      Result.Set_Background (R_Scen);
      if Errors then
         Result.Fail := True;
      end if;
      for I in Feature.Scenario_First .. Feature.Scenario_Last loop
         Process_Scenario (R_Scen, Feature.Scenario_Element (I),
                           Steps,
                           Log, Errors, Missing_Steps, Step_Matching);
         if Errors then
            Result.Fail := True;
         end if;
         Result.Scenario_Append (R_Scen);
      end loop;
      if Result.Fail then
         Log.Put_Line ("AdaSpec can create the procedures for you if you " &
                       "use --fill-steps");
      end if;
      Res := Result;
   end Process_Feature;

   ------------------------------------------
   --  Result_Feature_Type  --  To_String  --
   ------------------------------------------

   function  To_Code        (Res      : in     Result_Feature_Type;
                               Indent   : in     String := "")
                                          return String
   is
      CRLF   : constant String := "" & ASCII.LF;
      Buffer : Unbounded_String;
      S      : constant String := Res.Background.Name;
      E      : Result_Scenario_Type;
   begin
      Append (Buffer, Indent & "Feature " & Res.Name & CRLF);
      Append (Buffer, Indent & "   Background " & S & CRLF);
      Append (Buffer, Res.Background.To_Code (Indent & "      "));
      Append (Buffer, Indent & "   End Background " & S & CRLF);
      for I in Res.Scenario_First .. Res.Scenario_Last loop
         E := Res.Scenario_Element (I);
         Append (Buffer, Indent & "   Scenario " & E.Name & CRLF);
         Append (Buffer, E.To_Code (Indent & "      "));
         Append (Buffer, Indent & "   End Scenario " & E.Name & CRLF);
      end loop;
      Append (Buffer, Indent & "End Feature " & Res.Name & CRLF);
      return To_String (Buffer);
   end To_Code;

   ------------
   --  Fail  --
   ------------

   function  Fail (F : in Result_Feature_Type) return Boolean is
   begin
      return F.Fail;
   end Fail;

   ----------------
   --  Set_Fail  --
   ----------------

   procedure Set_Fail (F    : in out Result_Feature_Type;
                       Fail : in     Boolean := True) is
   begin
      F.Fail := Fail;
   end Set_Fail;

end AdaSpec.Result_Features;
