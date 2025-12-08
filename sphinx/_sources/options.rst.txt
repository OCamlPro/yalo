Configuration file options
==========================
Section "main"
--------------

* :code:`plugins` (String List): List of plugins to load (in this specific order)

* :code:`search_path` (String List): List of directories to search for plugin/profile files

* :code:`warnings` (String List): List of specifications to activate warnings

* :code:`ignore_namespaces` (StringSet): List of namespaces and tags to ignore in specifications

* :code:`errors` (String List): List of specifications to activate errors

* :code:`project` (String): The name of this project, starting from the root

* :code:`default_target` (String Option): The name of the project to lint by default

* :code:`fileattrs` ((String * fileattr) List): Associate attributes with files and folders

* :code:`profiles` (String List): List of profiles to use for this project (NAME means yalo-NAME.conf should be loaded)
Section "profiles"
------------------

* :code:`profile_plugins` (String List): List of plugins to load (in this specific order) for this profile

* :code:`profile_search_path` (String List): List of directories to search for plugin files for this profile

* :code:`profile_profiles` (String List): List of other profiles to use for this profile

* :code:`profile_fileattrs` ((String * fileattr) List): Associate attributes with files and folders

* :code:`profile_warnings` (String List): List of specifications to activate warnings. These specifications
  are used before the ones provided by the configuration, so that
  the project configuration has the final word.

* :code:`profile_errors` (String List): List of specifications to activate errors
