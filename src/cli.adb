--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO; use Ada.Text_IO;

package body CLI is

   procedure Help is
   begin

      --  (74)   0--------1--------2--------3--------4--------5--------6--------7----
      Put_Line ("xspec - generate tests from specification");
      Put_Line ("");
      Put_Line ("SYNOPSIS");
      Put_Line ("");
      Put_Line ("    xspec [OPTIONS] FEATURE ...");
      Put_Line ("");
      Put_Line ("DESCRIPTION");
      Put_Line ("");
      Put_Line ("    xspec takes as input feature files describing specifications");
      Put_Line ("    and generate tests for these features in languages such as Ada,");
      Put_Line ("    C, C++ depending on the template used.");
      Put_Line ("");
      Put_Line ("    Specifications are described in a formalized natural language");
      Put_Line ("    (see cucumber documentation) and the sentences are translated");
      Put_Line ("    in code using steps. These steps define a relation between the");
      Put_Line ("    regular expression matching the sentance and a function to");
      Put_Line ("    perform what the sentance describes.");
      Put_Line ("");
      Put_Line ("OPTIONS");
      Put_Line ("");
      Put_Line ("    FEATURE");
      Put_Line ("        The .feature file containing cucumber-like specification. It");
      Put_Line ("        is possible to specify multiple features on the command-line");
      Put_Line ("        all of them will be processed");
      Put_Line ("");
      Put_Line ("    -h, -help, --help");
      Put_Line ("        Help message");
      Put_Line ("");
      Put_Line ("    -t, --template TEMPLATE");
      Put_Line ("        Choose a template to use for code generation. It can be a");
      Put_Line ("        template name selected from a standard template directory or");
      Put_Line ("        a fully qualified path to a template file.");
      Put_Line ("");
      Put_Line ("    -T, -templatedir TEMPLATEDIR");
      Put_Line ("        Add TEMPLATEDIR to the list of template directory searched");
      Put_Line ("");
      Put_Line ("    -s, --step STEPDIR");
      Put_Line ("        Specify a step directory. By default, the step/ directory");
      Put_Line ("        relative to each feature file specified is searched.");
      Put_Line ("");
      Put_Line ("-- ");
      Put_Line ("Copyright (c) 2010 SOGILIS");
      --  (74)  0--------1--------2--------3--------4--------5--------6--------7----

   end Help;

end CLI;
