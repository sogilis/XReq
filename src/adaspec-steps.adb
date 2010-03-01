--                         Copyright (C) 2010, Sogilis                       --

with AdaSpec.Steps.Ada;

package body AdaSpec.Steps is

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
                   Directory : in     String;
                   Language  : in     Language_Type) is
   begin
      case Language is
         when Lang_Ada =>
            AdaSpec.Steps.Ada.Parse_Directory (Steps, Directory);
      end case;
   end Load;

   function  Load (Directory : in     String;
                   Language  : in     Language_Type) return Steps_Type is
      Result : Steps_Type;
   begin
      Load (Result, Directory, Language);
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
      use Step_Vectors;
      I    : Step_Vectors.Cursor := First (Steps);
      Step : Step_File_Ptr;
   begin
      while Has_Element (I) loop
         Step := Element (I);
         declare
            Result : constant String := Find (Step.all, Stanza);
         begin
            if Result /= "" then
               return Result;
            end if;
         end;
         Next (I);
      end loop;
      return "";
   end Find;

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
      while Has_Element (I) loop
         Step := Element (I);
         Find (Step.all, Stanza, Proc, Matches, Found2);
         if Found2 then
            Found := True;
            return;
         end if;
         Next (I);
      end loop;
      Found := False;
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
