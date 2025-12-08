About
=====

Yalo is a linting platform, written in OCaml. The philosophy of Yalo
is to be as extensible and customizable as possible by users. To
achieve this goal. Yalo makes an heavy use of plugins, so that users
can define their own plugins to define new warnings, and even to
define new languages to lint.

Main features of Yalo:

* All warnings and linters are defined in plugins
* Versioned interface for long term plugin support
* Project based configuration file (.yaloconf)
* Support for SARIF format for Github PR integration
* Warnings are enabled per project, but can be disabled locally. Easy
  way to select warnings by plugin, tag or number
* Linters are only called when the specific warnings they detect are
  enabled

Existing plugins:

* :code:`yalo_plugin_ocaml`: base support to lint OCaml programs. Allow
  users to define file linters, line linters, contents linters,
  parsetree linters (untyped, ppxlib) and typedtree linters (typed),
  applied on .ml, .mli, .cmi, .cmti and .cmt files.
* :code:`yalo_plugin_YALO`: line, typed and untyped linters for OCaml,
  developed for Yalo itsef. Based on :code:`yalo_plugin_ocaml`
* :code:`yalo_plugin_OCPINDENT`: linter for OCaml that checks indentation
  with ocp-indent. Based on :code:`yalo_plugin_ocaml`
* :code:`yalo_plugin_ZANUDA`: linters implementing most warnings of Zanuda
* :code:`yalo_plugin_CAMELOT`: linters implementing most warnings of Camelot

Extensions:

* :code:`ppx_yalo`: this ppx can be used to call Yalo at compile time. Typed
  linters are not available, unless an untyped version is provided.

Authors
-------

* Fabrice Le Fessant <fabrice.le_fessant@ocamlpro.com>
