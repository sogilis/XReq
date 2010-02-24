--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Fixed;

use Ada.Strings.Fixed;

package body Util.Strings is

   ------------------
   --  Find_Token  --
   ------------------

   procedure Find_Token (Search     : in String;
                         Tokens     : in String_List;
                         Index_Next : out Natural;
                         Token      : out Natural)
   is
      Idx      : Natural := 0;
      Last_Idx : Natural := 0;
      Last_Tk  : Natural := 0;
      Tk_Len   : Natural := 0;
   begin
      for Token_Index in Tokens'Range loop
         declare
            Token : constant String := To_String (Tokens (Token_Index));
         begin
            Idx := Index (Search, Token);
            if Idx /= 0 and (Last_Idx = 0 or Idx < Last_Idx) then
               Last_Idx := Idx;
               Last_Tk  := Token_Index;
               Tk_Len   := Token'Length;
            end if;
         end;
      end loop;
      if Last_Tk = 0 then
         Index_Next := 0;
         Token      := 0;
      else
         Index_Next := Last_Idx + Tk_Len;
         Token      := Last_Tk;
      end if;
   end Find_Token;

   -------------------
   --  Starts_With  --
   -------------------

   function Starts_With (Search      : in String;
                         Pattern     : in String;
                         Start_Index : in Natural := 1) return Boolean
   is
      sub : String (Pattern'First .. Pattern'Last);
   begin
      sub := Search (Start_Index .. Start_Index + Pattern'Length - 1);
      return sub = Pattern;
   exception
      when Constraint_Error =>
         return False;
   end Starts_With; --  GCOV_IGNORE

   ---------------------
   --  Trimed_Suffix  --
   ---------------------

   function Trimed_Suffix (Source      : in Unbounded_String;
                           Start_Index : in Natural) return Unbounded_String
   is
   begin
      return Unbounded_Slice (Source,
                              Index_Non_Blank (Source, Start_Index),
                              Length (Source));
   exception
      when Constraint_Error =>
         return Null_Unbounded_String;
   end Trimed_Suffix;


   function Trimed_Suffix (Source      : in String;
                           Start_Index : in Natural) return String
   is
   begin
      return Source (Index_Non_Blank (Source, Start_Index) .. Source'Last);
   exception
      when Constraint_Error =>
         return "";
   end Trimed_Suffix;

   ---------------------
   --  To_Identifier  --
   ---------------------

   function To_Identifier (Source : in String) return String is
      Buffer : Unbounded_String;
      Start  : Boolean := True;
      Char   : Character;
      Last   : Character;
   begin
      for i in Source'Range loop
         Char := Source (i);
         case Char is
            when 'a' .. 'z' | 'A' .. 'Z' =>
               Last := Char;
               Append (Buffer, Last);
               Start := False;
            when '0' .. '9' =>
               if not Start then
                  Last := Char;
                  Append (Buffer, Last);
               end if;
            when others =>
               if not Start and Last /= '_' then
                  Last := '_';
                  Append (Buffer, Last);
               end if;
         end case;
      end loop;
      if Last = '_' then
         Head (Buffer, Length (Buffer) - 1);
      end if;
      return To_String (Buffer);
   end To_Identifier;

   ------------------
   --  Ada_String  --
   ------------------

   function Ada_String    (Source : in String) return String is

      use Ada.Strings;
      procedure Start_String;
      procedure End_String;

      In_String : Boolean := True;
      Buffer    : Unbounded_String := To_Unbounded_String ("""");
      C         : Character;

      procedure Start_String is begin
         if not In_String then
            if Length (Buffer) /= 0 then
               Append (Buffer, " & ");
            end if;
            Append (Buffer, """");
            In_String := True;
         end if;
      end Start_String;

      procedure End_String is begin
         if In_String then
            Append (Buffer, """");
            In_String := False;
         end if;
      end End_String;
   begin
      for I in Source'Range loop
         C := Source (I);
         case C is
            when '"' =>
               Start_String;
               Append (Buffer, """""");
            when Character'Val (32) .. Character'Val (33)  |
                 Character'Val (35) .. Character'Val (126) =>
               Start_String;
               Append (Buffer, C);
            when others =>
               End_String;
               if Length (Buffer) /= 0 then
                  Append (Buffer, " & ");
               end if;
               Append (Buffer, "Character'Val (" &
                               Trim (Character'Pos (C)'Img, Left) & ")");
         end case;
      end loop;
      End_String;
      return To_String (Buffer);
   end Ada_String;

   --------------
   --  Buffer  --
   --------------

   procedure Put_Line (Buffer : in out Buffer_Type; S : in String) is
   begin
      Append (Buffer.Buffer, Buffer.Ind & S & Buffer.CRLF);
   end Put_Line;

   procedure Put_Line (Buffer : in out Buffer_Type; S : in Unbounded_String) is
   begin
      Append (Buffer.Buffer, Buffer.Ind & S & Buffer.CRLF);
   end Put_Line;

   procedure Put      (Buffer : in out Buffer_Type; S : in String) is
   begin
      Append (Buffer.Buffer, S);
   end Put;

   procedure Put      (Buffer : in out Buffer_Type; S : in Unbounded_String) is
   begin
      Append (Buffer.Buffer, S);
   end Put;

   procedure Put_Indent (Buffer : in out Buffer_Type) is
   begin
      Append (Buffer.Buffer, Buffer.Ind);
   end Put_Indent;

   procedure New_Line   (Buffer : in out Buffer_Type) is
   begin
      Append (Buffer.Buffer, Buffer.CRLF);
   end New_Line;

   procedure Indent   (Buffer : in out Buffer_Type; N : Natural := 3) is
   begin
      Append (Buffer.Ind, To_String (N * " "));
   end Indent;

   procedure UnIndent (Buffer : in out Buffer_Type; N : Natural := 3) is
   begin
      Head (Buffer.Ind, Length (Buffer.Ind) - N);
   end UnIndent;

   procedure Clear      (Buffer : in out Buffer_Type) is
   begin
      Buffer.Buffer := Null_Unbounded_String;
   end Clear;

   function  Value      (Buffer : in     Buffer_Type) return String is
   begin
      return To_String (Buffer.Buffer);
   end Value;


end Util.Strings;
