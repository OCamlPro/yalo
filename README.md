
[![Actions Status](https://github.com/lefessan/yalo/workflows/Main%20Workflow/badge.svg)](https://github.com/lefessan/yalo/actions)
[![Release](https://img.shields.io/github/release/lefessan/yalo.svg)](https://github.com/lefessan/yalo/releases)

# yalo

![Logo](./docs/logo.png)

This is the description
of the yalo OCaml project

* Website: https://lefessan.github.io/yalo
* General Documentation: https://lefessan.github.io/yalo/sphinx
* API Documentation: https://lefessan.github.io/yalo/doc
* Sources: https://github.com/lefessan/yalo

FAQ:

* How to improve pretty-printing of yalo errors ?

Yalo can output messages in SARIF JSON format, using
`--message-format=sarif`. You can then use tools for this format, such
as `sarif-fmt`:

```
yalo lint --message-format=sarif | sarif-fmt
```

* When I execute yalo and compile a local plugin, the execution always
fails with a Dynlink.Inconsistent_implementation ?

Check that the compiler that you are using is the one that was used to compile yalo. You may need to change the opam switch for example.
