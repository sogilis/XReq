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

   procedure Load (Steps     : in out Steps_Type;
                   Logger    : in     Logger_Ptr;
                   Directory : in     String;
                   Language  : in     Language_Type) is
   begin
      case Language is
         when Lang_Ada =>
            Logger.Put_Line ("Load Ada steps in: " & Directory);
            AdaSpec.Steps.Ada05.Parse_Directory (Steps, Directory);
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

   procedure Find      (Steps     : in  Steps_Type;
                        Stanza    : in  Stanza_Type;
                        Proc      : out Unbounded_String;
                        Matches   : out Match_Vectors.Vector;
                        Found     : out Boolean)
   is
      use Step_Vectors;
      I    : Step_Vectors.Cursor := First (Steps);
      Step : Step_File_Ptr;
      Found2 : Boolean := False;
   begin
      Found := False;
      while Has_Element (I) loop
         Step := Element (I);
         Find (Step.all, Stanza, Proc, Matches, Found2);
         if Found2 then
            if Found then
               raise Ambiguous_Match;
            else
               Found := True;
            end if;
         end if;
         Next (I);
      end loop;
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
