--                         Copyright (C) 2010, Sogilis                       --

package AdaSpec.Steps.Ada is

   type Ada_Step_Type is new Step_Type with private;
   type Ada_Step_Ptr  is access all Ada_Step_Type'Class;

   procedure Make (S         : in out Ada_Step_Type;
                   File_Name : in String);

   overriding function  Parsed    (S : in Ada_Step_Type) return Boolean;
   overriding procedure Parse     (S : in out Ada_Step_Type);

   overriding function  Contains  (S      : in Ada_Step_Type;
                                   Prefix : in Prefix_Type;
                                   Phrase : in String) return Boolean;

private

   type Ada_Step_Type is new Step_Type with
      record
         Parsed : Boolean := False;
      end record;

end AdaSpec.Steps.Ada;
