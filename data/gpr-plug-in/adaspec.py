#! python
# -*- coding: utf-8 -*-

"""
AdaSpec  -  Plugin for AdaSpec

This plugin use a special project file to configure AdaSpec: Makefile.adaspec
This is also a standard Makefile and its variables are used to configure the
AdaSpec project. When reading this file, no substitutions are made in variable
values.

Project variables:

  FEATURE_DIR:          directory where the .feature files are
  STEP_DEFINITIONS:     directory where the step definitions are
  RESULT_DIR:           where to generate the test suite
  TEST_SUITE:           name of the test suite
  GENERATED_STEPS:      package name where to put generated steps

Available commands for external use:

  def compile(filename = None):
    Compile the filename `filename' or the current project if not given

  def generate_steps(filename = None):
    Generate the step definitions

  def go_to_spec():
    Parse the current buffer with adaspec --partial --step-matching
    The result is used to jump to the step definition file

  def edit_makefile():
    Open in an editor the Makefile.adaspec project file.

  def show_feature_browser():
    Show the feature browser and refresh its content.
"""

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

import GPS
import gtk, gobject
import os
import re
import subprocess

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

def compile(filename = None):
  Command_AdaSpec(filename)

def generate_steps(filename = None):
  Command_AdaSpec(filename, run_gprbuild = False, generate_steps = True)

def go_to_spec():
  vars    = parse_makefile()
  context = GPS.current_context()
  file    = context.file()
  line_no = context.location().line()
  buffer  = GPS.EditorBuffer.get(file)
  #GPS.Console().write ("%s line %d\n" % (file.name(), line_no))
  if not buffer.is_modified():
    filename = file.name()
    delete   = False
  else:
    filename = GPS.dump(buffer.get_chars())
    delete   = True
  args = ["adaspec", "--partial", "--step-matching", "--step", vars['STEP_DEFINITIONS_DIR'], filename]
  p = subprocess.Popen(args, stdout=subprocess.PIPE)
  step_file = None
  step_line = None
  for line in p.stdout.readlines():
    m = re.match('^Step Matching: "(.*):([0-9]+)" matches "(.*):([0-9]+)" procedure (.*)$', line)
    if m:
      l = int(m.group(2))
      if l > line_no:
        break
      else:
        #GPS.Console().write ("line %d match %s (%s:%d)\n" % (
        #  int(m.group(2)), m.group(5), m.group(3), int(m.group(4))));
        step_file = m.group(3)
        step_line = int(m.group(4))
  p.stdout.close()
  if delete:
    os.remove(filename)
  if step_file:
    open_file(step_file, step_line)

def edit_makefile():
  open_file(create_makefile())

def show_feature_browser():
  win = GPS.MDI.get ('Features')
  if win:
    win.raise_window();
  else:
    GPS.MDI.get ("Project").raise_window()
    view = FeatureBrowser ()
    GPS.MDI.add (view, "Features", "Features")
    win = GPS.MDI.get ('Features')
    #win.float(True)
    #GPS.MDI.get ("Project").raise_window()
    #win.float(False)
    win.raise_window()
    win.split (False)


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
##                         Makefile related functions                         ##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

def makefile_path():
  return os.path.join(os.path.dirname(GPS.Project.root().file().name()), "Makefile.adaspec")


def create_makefile():
  file = makefile_path()
  if not os.path.isfile(file):
    f = open(file, "w")
    f.write ("""
############################
##  AdaSpec Project File  ##
############################

# FEATURE_DIR:          directory where the .feature files are
# STEP_DEFINITIONS:     directory where the step definitions are
# RESULT_DIR:           where to generate the test suite
# TEST_SUITE:           name of the test suite
# GENERATED_STEPS:      package name where to put generated steps

FEATURE_DIR=features
STEP_DEFINITIONS_DIR=features/step_definitions
RESULT_DIR=features/tests
TEST_SUITE=test_suite
GENERATED_STEPS=Generated_Steps

all: $(RESULT_DIR)/$(TEST_SUITE)
.PHONY: all

$(RESULT_DIR)/$(TEST_SUITE): $(RESULT_DIR)/$(TEST_SUITE).gpr
        gprbuild -m -P$<

$(RESULT_DIR)/$(TEST_SUITE).gpr: $(FEATURE_DIR)/*.feature
        adaspec -x $(TEST_SUITE) $+

clean:
        -$(RM) $(RESULT_DIR)/$(TEST_SUITE).{gpr,adb}
        -$(RM) $(RESULT_DIR)/feature_*.{ads,adb}
.PHONY: clean

    """)
    f.close()
  return file

def parse_makefile():
  file = create_makefile()
  #GPS.Console().write("Parse %s\n" % file)
  res = {
    'FEATURE_DIR'         : "features",
    'STEP_DEFINITIONS_DIR': "features/steps",
    'RESULT_DIR'          : "features/tests",
    'TEST_SUITE'          : "",
    'GENERATED_STEPS'     : "Generated_Steps"}
  f = open (file, "r")
  for line in f.readlines():
    var, eq, val = line.partition("=")
    var = var.strip()
    val = val.strip()
    if eq == "=" and re.match("[A-Z][A-Z0-9_]*", var):
      res[var] = val
      #GPS.Console().write("%s=%s\n" % (var, val))
  f.close()
  return res

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
##                          Editor related functions                          ##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

def open_file(file, line=None, always_open=False):
  if line:
    GPS.Console().write ("Open %s line %d\n" % (file, line))
  else:
    GPS.Console().write ("Open %s\n" % file)
  if not always_open and not line and file.endswith(".gpr"):
    GPS.Project.load(file);
  else:
    ed = GPS.EditorBuffer.get (GPS.File (file))
    GPS.MDI.get_by_child (ed.current_view()).raise_window()
    if line:
      ed.current_view().goto(GPS.EditorLocation (ed, line, 0) )

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
##                              Launch  gprbuild                              ##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

class Command_GprBuild (GPS.Process):
  location_category = "Build test suite"
  def on_match (self, matched, unmatched):
    matched = re.sub("^\n*", "", matched)
    matched = re.sub("\n+", "\n", matched)
    for line in matched.split("\n"):
      line = "%s/%s" % (self.vars['RESULT_DIR'], line)
      GPS.Locations.parse(line,
                          self.location_category,
                          regexp = "^(.*)\:([0-9]+)\:([0-9]+)\: +(.*)$",
                          file_index = 1,
                          line_index = 2,
                          column_index = 3,
                          msg_index = 4)
    GPS.Console().write(matched)

  def __init__ (self, projectfile, vars = None):
    if not vars: vars = parse_makefile()
    self.vars = vars
    GPS.Console().write("\n\n")
    GPS.Locations.remove_category(self.location_category);
    GPS.Process.__init__ (self, 'gprbuild -m -vP0 -d """-P%s"""' % projectfile,
      show_command = True,
      regexp = "^.+$",
      single_line_regexp = True,
      on_match = self.on_match,
      progress_regexp = "^completed ([0-9]+) out of ([0-9]+).*$",
      progress_current = 1,
      progress_total   = 2)


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
##                               Launch adaspec                               ##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

class Command_AdaSpec(GPS.Process):
  location_category = "Build features"
  def on_match (self, matched, unmatched):
    matched = re.sub("^\n*", "", matched)
    matched = re.sub("\n+", "\n", matched)
    for line in matched.split("\n"):
      GPS.Locations.parse(line,
                          self.location_category,
                          regexp = "^(.*)\:([0-9]+)\: ERROR: +(.*)$",
                          file_index = 1,
                          line_index = 2,
                          msg_index = 3)
    GPS.Console().write(matched)

  def on_exit (self, status, output):
    GPS.Console().write(output)
    if self.run_gprbuild and self.vars['TEST_SUITE']:
      gprpath = "%s/%s.gpr" % (self.vars['RESULT_DIR'], self.vars['TEST_SUITE'])
      Command_GprBuild(gprpath, self.vars)

  def __init__ (self, filename=None, run_gprbuild=True, vars=None, generate_steps=False):
    if not vars: vars = parse_makefile()
    self.run_gprbuild = run_gprbuild
    self.vars = vars

    GPS.Console().write("\n\n")
    GPS.Locations.remove_category(self.location_category);

    command="adaspec --progress --keep-going --quiet";
    if vars['TEST_SUITE'] and not filename:
      command = command + ' --executable """%s"""' % vars['TEST_SUITE']
    command = command + ' --step """%s""" ' % vars['STEP_DEFINITIONS_DIR']
    command = command + ' --output """%s"""' % vars['RESULT_DIR']
    if generate_steps:
      command = command + ' --fill-steps-in """%s"""' % vars['GENERATED_STEPS']
    if filename:
      command = command + ' """%s"""' % filename
    else:
      for f in os.listdir(vars['FEATURE_DIR']):
        fullpath = os.path.join(vars['FEATURE_DIR'], f)
        if f.endswith(".feature") and os.path.isfile(fullpath):
          command = command + ' """%s"""' % fullpath

    GPS.Process.__init__ (self, command,
      show_command = True,
      regexp = "^.+$",
      single_line_regexp = True,
      on_match = self.on_match,
      on_exit  = self.on_exit,
      progress_regexp = "^completed ([0-9]+) out of ([0-9]+)$",
      progress_current = 1,
      progress_total   = 2)


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
##                         Feature Browser  side view                         ##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

class FeatureBrowser(gtk.Table):
  def __init__(self):
    gtk.Table.__init__(self, rows=2, columns=1);
    self.toolBar = gtk.Toolbar()

    btn = gtk.ToolButton()
    btn.set_icon_name(gtk.STOCK_REFRESH)
    btn.set_label("Refresh")
    btn.set_tooltip_text("Refresh the feature view")
    btn.connect ('clicked', lambda x: self.refresh())
    self.toolBar.insert(btn, 1)

    btn = gtk.ToolButton()
    btn.set_icon_name(gtk.STOCK_EDIT)
    btn.set_label("Edit project file")
    btn.set_tooltip_text("Edit the AdaSpec Makefile")
    btn.connect ('clicked', lambda x: edit_makefile())
    self.toolBar.insert(btn, 2)

    btn = gtk.ToolButton()
    btn.set_icon_name(gtk.STOCK_EXECUTE)
    btn.set_label("Compile")
    btn.set_tooltip_text("Compile features")
    btn.connect ('clicked', lambda x: compile())
    self.toolBar.insert(btn, 3)

    btn = gtk.ToolButton()
    btn.set_icon_name(gtk.STOCK_CONVERT)
    btn.set_label("Generate Steps")
    btn.set_tooltip_text("Generate missing step definitions")
    btn.connect ('clicked', lambda x: (generate_steps(), compile()))
    self.toolBar.insert(btn, 4)

    self.attach(self.toolBar, 0, 1, 0, 1, yoptions=0)
    self.treeModel = gtk.TreeStore(gtk.gdk.Pixbuf, gobject.TYPE_STRING, gobject.TYPE_STRING)
    self.populateModel();
    self.treeView = gtk.TreeView(self.treeModel);
    col = gtk.TreeViewColumn()
    self.treeView.append_column(col);
    cell = gtk.CellRendererPixbuf()
    col.pack_start(cell, False)
    col.add_attribute(cell, 'pixbuf', 0)
    cell = gtk.CellRendererText()
    col.pack_start(cell, True)
    col.add_attribute(cell, 'text', 1)
    self.treeView.set_search_column(1)
    self.treeView.set_headers_visible(False)
    self.treeView.connect("row-activated", self.feature_activated)
    scroll = gtk.ScrolledWindow()
    scroll.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
    scroll.add(self.treeView)
    self.attach(scroll, 0, 1, 1, 2)

  def populateModel(self):
    vars = parse_makefile()
    feature_dir = vars['FEATURE_DIR']
    steps_dir = vars['STEP_DEFINITIONS_DIR']
    result_dir = vars['RESULT_DIR']
    self.treeModel.clear()
    img_d = gtk.icon_theme_get_default().load_icon("folder", gtk.ICON_SIZE_MENU, 0)
    img_f = gtk.icon_theme_get_default().load_icon("document", gtk.ICON_SIZE_MENU, 0)
    if os.path.isdir(feature_dir):
      it = self.treeModel.append(None, [img_d, "Features", feature_dir]);
      for f in os.listdir(feature_dir):
        fullname = os.path.join(feature_dir, f)
        if f.endswith (".feature") and os.path.isfile(fullname):
          self.treeModel.append(it, [img_f, f, fullname])
    if os.path.isdir(steps_dir):
      it = self.treeModel.append(None, [img_d, "Step Definitions", steps_dir]);
      for f in os.listdir(steps_dir):
        fullname = os.path.join(steps_dir, f)
        if (f.endswith (".adb") or f.endswith (".ads")) and os.path.isfile(fullname):
          self.treeModel.append(it, [img_f, f, fullname])
    if os.path.isdir(result_dir):
      it = self.treeModel.append(None, [img_d, "Generated Files", result_dir]);
      for f in os.listdir(result_dir):
        fullname = os.path.join(result_dir, f)
        if (f.endswith (".adb") or f.endswith (".ads") or f.endswith (".gpr")) and os.path.isfile(fullname):
          self.treeModel.append(it, [img_f, f, fullname])

  def refresh(self):
    self.populateModel()

  def feature_activated(self, tree, path, view_col, *user):
    model = tree.get_model()
    path = model.get_value(model.get_iter(path), 2)
    open_file(path)


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
##                                   Hooks                                   .##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

def on_project_changed(hook):
  if os.path.isfile(makefile_path()):
    show_feature_browser()

GPS.Hook ("project_changed").add (on_project_changed)