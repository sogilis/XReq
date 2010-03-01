--                         Copyright (C) 2010, Sogilis                       --

package AdaSpec.Lang is

   Invalid_Language : exception;

   type Language_Type is (Lang_Ada);

   function Get_Language (Lang : in String) return Language_Type;

end AdaSpec.Lang;