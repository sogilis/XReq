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

with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Hashed_Sets;

use Ada.Strings.Unbounded;

package Util.Strings is

   type String_List is  --  GCOV_IGNORE
      array (Positive range <>) of Unbounded_String;

   package String_Sets is
      new Ada.Containers.Hashed_Sets (Unbounded_String, Hash, "=", "=");

   subtype String_Set is String_Sets.Set;

   function  Replace      (Subject     : in String;
                           Pattern     : in String;
                           Replacement : in String) return String;

   procedure Replace      (Subject     : in out Unbounded_String;
                           Pattern     : in String;
                           Replacement : in String);

   procedure Find_Token   (Search     : in String;
                           Tokens     : in String_List;
                           Index_Next : out Natural;
                           Token      : out Natural);

   function Starts_With   (Search      : in String;
                           Pattern     : in String;
                           Start_Index : in Natural := 1) return Boolean;

   function Trimed_Suffix (Source      : in Unbounded_String;
                           Start_Index : in Natural) return Unbounded_String;

   function Trimed_Suffix (Source      : in String;
                           Start_Index : in Natural) return String;

   function To_Identifier (Source  : in String) return String;  --  GCOV_IGNORE

   function To_Package_File_Id
                          (Source  : in String) return String;

   function Ada_String    (Source  : in String) return String;

   function C_String      (Source  : in String) return String;

   function Decode_Python (Source  : in String;
                           Liberal : in Boolean := False)
                                 return Unbounded_String;

   function Decode_Python (Source  : in String;
                           Liberal : in Boolean := False) return String;

   function Decode_String (Source  : in String) return Unbounded_String;
   function Decode_String (Source  : in String) return String;

   function Relative_Path (Origin  : in String;
                           Target  : in String) return String;

   function Reverse_Path  (Path    : in String) return String;

   function Goto_Path     (Origin  : in String;
                           Target  : in String) return String;

   type Split_String_Walker is access procedure (S : in String);

--    procedure Split_String_Walk
--                           (Str     : in String;
--                            Split   : in String;
--                            Walker  : in Split_String_Walker);

   --  The following is quite ugly, but you know, subprogram must not be deeper
   --  than access type

   type Split_String_Type is
      record
         Str   : Unbounded_String;
         Split : Unbounded_String;
         I, J  : Natural;
      end record;

   procedure Split_String_Start (Splitter : out Split_String_Type;
                                 Str      : in String;
                                 Split    : in String);

   procedure Split_String_Next  (Splitter : in out Split_String_Type);

   function  Split_String_Current (Splitter : in Split_String_Type)
                                            return String;

   function  Split_String_Has_Next (Splitter : in Split_String_Type)
                                            return Boolean;

   --------------
   --  Buffer  --
   --------------

   type Buffer_Type is tagged
      record
         Buffer : Unbounded_String;
         Ind    : Unbounded_String;
         CRLF   : Unbounded_String := To_Unbounded_String ("" & ASCII.LF);
      end record;

   procedure Put_Line   (Buffer : in out Buffer_Type; S : in String);
   procedure Put_Line   (Buffer : in out Buffer_Type; S : in Unbounded_String);
   procedure Put        (Buffer : in out Buffer_Type; S : in String);
   procedure Put        (Buffer : in out Buffer_Type; S : in Unbounded_String);
   procedure Put_Indent (Buffer : in out Buffer_Type);
   procedure New_Line   (Buffer : in out Buffer_Type);
   procedure Indent     (Buffer : in out Buffer_Type; N : in Natural := 3);
   procedure UnIndent   (Buffer : in out Buffer_Type; N : in Natural := 3);
   procedure Clear      (Buffer : in out Buffer_Type);
   function  --  GCOV_IGNORE
             Value      (Buffer : in     Buffer_Type) return String;

end Util.Strings;
