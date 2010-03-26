--                         Copyright (C) 2010, Sogilis                       --

with AdaSpec.Steps.Ada05;

package body AdaSpec.Steps is

   ----------------
   --  Contains  --
   ----------------

   function  Contains  (S       : in  Step_File_Type;
                        Stanza  : in  Stanza_Type) return Boolean
   is
      This : constant access constant Step_File_Type'Class := S'Access;
   begin
      return This.Find (Stanza) /= "";
   end Contains;

   ------------
   --  Find  --
   ------------

   function  Find      (S       : in  Step_File_Type;
                        Stanza  : in  Stanza_Type) return String
   is
      This    : constant access constant Step_File_Type'Class := S'Access;
      Proc    : Unbounded_String;
      Matches : Match_Vectors.Vector;
      Found   : Boolean;
   begin
      This.Find (Stanza, Proc, Matches, Found);
      if Found then
         return To_String (Proc);
      else
         return "";
      end if;
   end Find;

   ------------
   --  Find  --
   ------------

   procedure Find      (S       : in  Step_File_Type;
                        Stanza  : in  Stanza_Type;
                        Proc    : out Unbounded_String;
                        Matches : out Match_Vectors.Vector;
                        Found   : out Boolean)
   is
      This   : constant access constant Step_File_Type'Class := S'Access;
      Result : constant Step_Match_Type := This.Find (Stanza);
   begin
      Found   := Result.Match;
      Proc    := Result.Proc_Name;
      Matches := Result.Matches;
   end Find;

   -----------------
   --  File_Name  --
   -----------------

   function  File_Name (S : in Step_File_Type) return String is
   begin
      return To_String (S.File_Name);
   end File_Name;

   ------------
   --  Load  --
   ------------

   procedure Load (Steps      : in out Steps_Type;
                   Logger     : in     Logger_Ptr;
                   Directory  : in     String;
                   Language   : in     Language_Type;
                   Fill_Steps : in     Boolean := False) is
   begin
      case Language is
         when Lang_Ada =>
            Logger.Put_Line ("Load Ada steps in: " & Directory);
            AdaSpec.Steps.Ada05.Parse_Directory
               (Steps, Logger, Directory, Fill_Steps);
      end case;
   end Load;

   function  Load (Directory : in     String;
                   Language  : in     Language_Type) return Steps_Type is
      Result : Steps_Type;
   begin
      Load (Result, Null_Logger, Directory, Language);
      return Result;
   end Load;

   ----------------
   --  Contains  --
   ----------------

   function  Contains  (Steps     : in Steps_Type;
                        Stanza    : in Stanza_Type) return Boolean
   is
   begin
      return Find (Steps, Stanza) /= "";
   end Contains;

   ------------
   --  Find  --
   ------------

   function  Find      (Steps     : in Steps_Type;
                        Stanza    : in Stanza_Type) return String
   is
      Proc    : Unbounded_String;
      Matches : Match_Vectors.Vector;
      Found   : Boolean;
   begin
      Find (Steps, Stanza, Proc, Matches, Found);
      if Found then
         return To_String (Proc);
      else
         return "";
      end if;
   end Find;  --  GCOV_IGNORE

   ------------
   --  Find  --
   ------------

   function  Find      (Steps     : in Steps_Type;
                        Stanza    : in Stanza_Type) return Step_Match_Type
   is
      use Step_Vectors;
      Result : Step_Match_Type;
      I      : Step_Vectors.Cursor := First (Steps);
      Step   : Step_File_Ptr;
      Res2   : Step_Match_Type;
   begin
      while Has_Element (I) loop
         Step := Element (I);
         Res2 := Find (Step.all, Stanza);
         if Res2.Match then
            if Result.Match then
               raise Ambiguous_Match;
            else
               Result := Res2;
            end if;
         end if;
         Next (I);
      end loop;
      return Result;
   end Find;  --  GCOV_IGNORE

   ------------
   --  Find  --
   ------------

   procedure Find      (Steps     : in  Steps_Type;
                        Stanza    : in  Stanza_Type;
                        Proc      : out Unbounded_String;
                        Matches   : out Match_Vectors.Vector;
                        Found     : out Boolean)
   is
      Res : constant Step_Match_Type := Find (Steps, Stanza);
   begin
      Found := Res.Match;
      if Found then
         Proc    := Res.Proc_Name;
         Matches := Res.Matches;
      end if;
   end Find;

   ------------
   --  Free  --
   ------------

   procedure Free (Steps : in out Steps_Type) is
      use Step_Vectors;
      I : Step_Vectors.Cursor := First (Steps);
      E : Step_File_Ptr;
   begin
      while Has_Element (I) loop
         E := Element (I);
         E.Finalize;
         Free (E);
         Next (I);
      end loop;
      Clear (Steps);
   end Free;


end AdaSpec.Steps;
