
Sub-commands and Arguments
==========================

Overview of sub-commands::
  
  doc
    Command to generate the JSON documentation of the currently loaded plugins
  
  lint
    Lint a project or a list of files.


yalo doc
~~~~~~~~~~

Command to generate the JSON documentation of the currently loaded plugins



**DESCRIPTION**




**USAGE**
::
  
  yalo doc [OPTIONS]

Where options are:


* :code:`-C CONFIG-FILE` or :code:`--config-file CONFIG-FILE`   Load CONFIG-FILE instead of searching for .yaloconf

* :code:`-I DIR` or :code:`--include-dir DIR`   Add DIR to the list of directories when plugins should be searched for

* :code:`-L PLUGIN` or :code:`--load-plugin PLUGIN`   Load plugin PLUGIN (a .cmxs or a .ml file)

* :code:`-P PROFILE` or :code:`--profile PROFILE`   Specify a profile to load (a yalo-<PROFILE>.conf file)

* :code:`--dir DIRECTORY`   Target directory for output

* :code:`-e SPEC` or :code:`--errors SPEC`   Set errors according to SPEC-ification

* :code:`--no-load-plugins`   Do not load plugins

* :code:`--print-config`   Print configuration

* :code:`--save-config FILE`   Save configuration to FILE

* :code:`-w SPEC` or :code:`--warnings SPEC`   Set warnings according to SPEC-ification


yalo lint
~~~~~~~~~~~

Lint a project or a list of files.



**DESCRIPTION**


Thie command will perform the following actions

Early actions (common to all sub-commands):

* 1.
  Lookup the .yaloconf file in the containing folders. if located, chdir to the corresponding directory.

* 2.
  If a configuration file was found, load the corresponding file. If profiles are specified in the configuration file, recursively load the profiles too.

* 3.
  If plugins are specified on command line, in the configuration file or in profiles specified in the configuration file, load the plugins

Specific actions:

* a.
  Enable/disable warnings following command line and configuration options. Enable only linters for enabled warnings.

* b.
  Scan the project tree, looking for files to lint. Each file is associated with a set of including projects.

* c.
  Lint all the files of selected projects

* d.
  Display or output warnings

* e.
  Apply autofix patches if available and the --autofix option was used


**INITIAL ARGUMENTS**


Some arguments MUST be specified before the sub-command name (-L,-P,-I,-C,--no-load-plugins). The reason is that these arguments are used to define which and how plugins should be loaded, either directly or though configuration files, and plugins can define new arguments for sub-commands and even new sub-commands

**USAGE**
::
  
  yalo lint FILES [OPTIONS]

Where options are:


* :code:`FILES`   List of files or directories that should be explicitely linted

* :code:`-C CONFIG-FILE` or :code:`--config-file CONFIG-FILE`   Load CONFIG-FILE instead of searching for .yaloconf

* :code:`-I DIR` or :code:`--include-dir DIR`   Add DIR to the list of directories when plugins should be searched for

* :code:`-L PLUGIN` or :code:`--load-plugin PLUGIN`   Load plugin PLUGIN (a .cmxs or a .ml file)

* :code:`-P PROFILE` or :code:`--profile PROFILE`   Specify a profile to load (a yalo-<PROFILE>.conf file)

* :code:`--autofix`   Apply all automatic replacements (files created in _yalo/)

* :code:`--autofix-inplace`   Autofix files in place

* :code:`-e SPEC` or :code:`--errors SPEC`   Set errors according to SPEC-ification

* :code:`-f FORMAT` or :code:`--message-format FORMAT`   Set message format to FORMAT: context (default), human, short, summary, sarif

* :code:`--no-load-plugins`   Do not load plugins

* :code:`--no-summary`   Never print any summary

* :code:`-o FILE` or :code:`--output FILE`   File for JSON output

* :code:`-p PROJECT` or :code:`--package PROJECT`   Lint only files from PROJECT

* :code:`--print-config`   Print configuration

* :code:`--save-config FILE`   Save configuration to FILE

* :code:`--skip-config-warnings`   Skip warnings and errors settings by config file

* :code:`--summary-from NUMBER`   Print summary when warnings exceed NUMBER

* :code:`-w SPEC` or :code:`--warnings SPEC`   Set warnings according to SPEC-ification
