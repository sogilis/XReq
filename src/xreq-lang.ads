--                         Copyright (C) 2010, Sogilis                       --

package XReq.Lang is

   Invalid_Language : exception;

   type Language_Type is (Lang_Ada, Lang_C);

   function Get_Language (Lang : in String) return Language_Type;

end XReq.Lang;