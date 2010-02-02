--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO; use Ada.Text_IO;

package body CLI is

   procedure Help is
   begin

      Put_Line ("adaspec2 - generate tests from specification");
      Put_Line ("");
      Put_Line ("SYNOPSIS");
      Put_Line ("");
      Put_Line ("    adaspec2 [OPTIONS] FEATURE ...");
      Put_Line ("");
      Put_Line ("DESCRIPTION");
      Put_Line ("");
      Put_Line ("    adaspec2 takes as input feature files describing");
      Put_Line ("    specificationsand generate tests for these features in");
      Put_Line ("    languages such as Ada, C, C++ depending on the template");
      Put_Line ("    used.");
      Put_Line ("");
      Put_Line ("    Specifications are described in a formalized natural");
      Put_Line ("    language (see cucumber documentation) and the sentences");
      Put_Line ("    are translated in code using steps. These steps define");
      Put_Line ("    a relation between the regular expression matching the");
      Put_Line ("    sentance and a function toperform what the sentance");
      Put_Line ("    describes.");
      Put_Line ("");
      Put_Line ("OPTIONS");
      Put_Line ("");
      Put_Line ("    FEATURE");
      Put_Line ("        The .feature file containing cucumber-like");
      Put_Line ("        specification. It is possible to specify multiple");
      Put_Line ("        features on the command-line all of them will be");
      Put_Line ("        processed");
      Put_Line ("");
      Put_Line ("    -h, -help, --help");
      Put_Line ("        Help message");
      Put_Line ("");
      Put_Line ("    -t, --template TEMPLATE");
      Put_Line ("        Choose a template to use for code generation. It");
      Put_Line ("        can be a template name selected from a standard");
      Put_Line ("        template directory or a fully qualified path to a");
      Put_Line ("        template file.");
      Put_Line ("");
      Put_Line ("    -T, -templatedir TEMPLATEDIR");
      Put_Line ("        Add TEMPLATEDIR to the list of template directory");
      Put_Line ("        searched");
      Put_Line ("");
      Put_Line ("    -s, --step STEPDIR");
      Put_Line ("        Specify a step directory. By default, the step/");
      Put_Line ("        directory relative to each feature file specified");
      Put_Line ("        is searched.");
      Put_Line ("");
      Put_Line ("-- ");
      Put_Line ("Copyright (c) 2010 SOGILIS");

   end Help;

end CLI;
