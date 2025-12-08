Configuration file format
=========================

Configuration file
------------------

For any project, the configuration is located in a file
:code:`.yaloconf` at the root of the project. Yalo will chdir to this
directory on launch.

It is possible to ask Yalo to generate (or update) the configuration
file with the option :code:`--save-config OUTPUT-FILE`. It can be
useful to complete an existing configuration file with additional
options and their default values.

Configuration profiles
----------------------

The configuration file can specify *profiles* to load: a profile
:code:`<name>` will trigger the load of a configuration file
:code:`yalo-<name>.conf`.

A profile can set both standard options and profile options:

* If a standard option has been set in the configuration file, or in a
  previously loaded profile, the option cannot be change by the
  profile.

* A profile option starts with :code:`profile_` and is in the
  :code:`profile` section of the configuration file. When a profile
  option is set by a profile, the value is actually concatenated to
  the previous value.


General format of .yaloconf
---------------------------

The following formatting rules are used in the configuration file:

* Comments use the OCaml notation :code:`(* ... *)`
* Options are specified with: :code:`name = value`
* Options can be nested into records: :code:`name1 = { name2 = value }`
  should be used to specify option :code:`name1.name2`
* Line breaks are equivalent to spaces

Values are defined with the following expressions:

* Booleans: :code:`true` and :code:`false`
* Integers: :code:`1383` for example
* Strings: :code:`"Hello world"`, or :code:`Hello` if no special character
* Lists/tuples: :code:`[ value1 ; value2 ; value3 ]` or
  :code:`( value1, value2, value3 )` (:code:`;` and :code:`,` are not always
  necessary)
* Records: :code:`{ x = 1 y = 2 }`

Format of file attributes (fileattr)
------------------------------------

The options :code:`fileattrs` and :code:`profile_fileattrs` have a
specific format: it is a list of pairs, associating a :code:`fileattr`
record to a path in the project.

The :code:`fileattr` record can have the following fields:

* :code:`skip` (boolean): skip this folder from scanning, or skip this
  file from linting;
* :code:`project` (string list): a list of projects to which this file
  or folder belongs to. A single string may contain multiple
  comma-separated projects;
* :code:`tag` (string). A tag to associate with the files in the
  folder (not used yet);
* :code:`ignore` (string list): a list of files or folders to ignore
  in this folder;

Format of warning/error specs
-----------------------------

It is possible to enable/disable warnings individually, either
globally for the project, or locally in a file. The :code:`warnings`
list indicates which warnings should be enabled (thus enabling the
corresponding linters), and :code:`errors` which warnings should
actually be treated as errors (making Yalo exit with an error code).

It may happen that warnings should be only enabled in some files, yet
the linters should be active. Yalo calls these warnings *sleeping*
warnings, that can be awaken in a file.

It may also happen that warnings shouldn't be disabled locally by
developers. Yalo calls these warnings *forced* warnings, that can
never be disabled locally.

The specification is a list of strings, each string being a
comma-separated list of :code:`spec`:

::
   
   SPEC :=
   | + (* activate all existing warnings *)
   | - (* disactivate all existing warnings *)
   | #tag | +#tag  (* activate all warnings with this tag *)
   | -#tag        (* disactivate all warnings with this tag *)
   | NS_SPEC W_SPEC*
   
   NS_SPEC =
   | NS (* all following warnings are in this plugin. If no
           W_SPEC follows, then +NS is understood. *)
   | +NS (* activate all warnings of this plugin and use it for next warnings *)
   | -NS (* disactivate all warnings of this plugin and
   use it for next warnings *)
   
   W_SPEC =
   | +#tag (* activate all warnings with this tag in the plugin *)
   | -#tag (* disactivate all warnings with this tag in the plugin *)
   | +num (* activate the warning with this number in the plugin *)
   | -num (* disactivate the warning with this number in the plugin *)
   
   '?' can be used instead of '+/-' to make a warning possible, but not
     activated by default (can be enabled locally).
   '!' can be used instead of '+/-' to make a warning forced, ie enabled
     without the possibility to disable it locally.

