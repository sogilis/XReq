--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with XReqLib.String_Tables;

use  Ada.Strings.Unbounded;
use  Ada.Containers;

package XReqLib.Args is

   ----------------
   --  Arg_Type  --
   ----------------

   package Table_Pkg    renames XReqLib.String_Tables;
   subtype Table_Type   is XReqLib.String_Tables.Table;
   subtype Table_Cursor is XReqLib.String_Tables.Cursor;

   type Arg_Type is tagged private;
   Null_Arg : constant Arg_Type;

   type Arg_Element_Type is                                     --  GCOV_IGNORE
      (Arg_Text, Arg_Table, Arg_Separator, Arg_Paragraph);      --  GCOV_IGNORE

   procedure Make         (Self   : out    Arg_Type;
                           Stanza : in     String);
   function  Match        (Self   : in     Arg_Type;
                           N      : in     Natural)  return String;
   procedure Match        (Self   : in     Arg_Type;
                           N      : in     Natural;
                           First  : out    Natural;
                           Last   : out    Natural);
   function  Stanza       (Self   : in     Arg_Type) return String;
   function  First_Match  (Self   : in     Arg_Type) return Natural;
   function  Last_Match   (Self   : in     Arg_Type) return Natural;
   procedure Add_Match    (Self   : in out Arg_Type;
                           First  : in     Natural;
                           Last   : in     Natural);
   --  Text  --
   function  Text         (Self   : in     Arg_Type;
                           N      : in     Natural := 0) return String;
   procedure Add_Text     (Self   : in out Arg_Type;
                           Text   : in     String);
   function  First_Text   (Self   : in     Arg_Type) return Natural;
   function  Last_Text    (Self   : in     Arg_Type) return Integer;
   function  Num_Text     (Self   : in     Arg_Type) return Natural;
   --  Table  --
   function  Table        (Self   : in     Arg_Type;
                           N      : in     Natural := 0) return Table_Type;
   procedure Add_Table    (Self   : in out Arg_Type;
                           Table  : in     Table_Type);
   function  First_Table  (Self   : in     Arg_Type) return Natural;
   function  Last_Table   (Self   : in     Arg_Type) return Integer;
   --  Separator  --
   procedure Add_Sep      (Self   : in out Arg_Type;
                           N      : in     Natural := 0);
   --  Paragraph  --
   function  Para         (Self   : in     Arg_Type;
                           N      : in     Natural := 0) return String;
   procedure Add_Para     (Self   : in out Arg_Type;
                           Text   : in     String);
   function  First_Para   (Self   : in     Arg_Type) return Natural;
   function  Last_Para    (Self   : in     Arg_Type) return Integer;
   --  Element  --
   function  First        (Self   : in     Arg_Type) return Natural;
   function  Last         (Self   : in     Arg_Type) return Integer;
   function  Elem_Type    (Self   : in     Arg_Type;
                           N      : in     Natural)  return Arg_Element_Type;
   function  Elem_Idx     (Self   : in     Arg_Type;
                           N      : in     Natural)  return Natural;

private

   type Match_Type is  --  GCOV_IGNORE
      record
         First : Natural;
         Last  : Natural;
      end record;

   type Arg_Element_Record is
      record
         Element_Type : Arg_Element_Type;
         Element_Idx  : Natural := 0;
      end record;

   package Elem_Vectors is new Vectors (Natural, Arg_Element_Record, "=");
   package Match_Vectors is new Vectors (Positive, Match_Type, "=");
   package String_Vectors is new Vectors (Natural, Unbounded_String, "=");
   package Table_Vectors is new Vectors (Natural, Table_Type,
                                         XReqLib.String_Tables."=");

   type Arg_Type is tagged
      record
         Stanza     : Unbounded_String;
         Matches    : Match_Vectors.Vector;
         Elems      : Elem_Vectors.Vector;
         Texts      : String_Vectors.Vector;
         Tables     : Table_Vectors.Vector;
         Paragraphs : String_Vectors.Vector;
      end record;

   Null_Arg : constant Arg_Type := (others => <>);

end XReqLib.Args;
