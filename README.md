
[![Actions Status](https://github.com/OCamlPro/yalo/workflows/Main%20Workflow/badge.svg)](https://github.com/OCamlPro/yalo/actions)
[![Release](https://img.shields.io/github/release/OCamlPro/yalo.svg)](https://github.com/OCamlPro/yalo/releases)

# yalo

![Logo](./docs/logo.png)

Yalo aims at providing a code scanner for OCaml projects. Because most organizations want to easily add their own linting rules, Yalo is plugin-based, it provides a simple to use interface to easily write such plugins. Projects can easily be configured to use Yalo, activate the warnings/errors that they want, and integrate the tool in Github Actions thanks to SARIF JSON output.

* Website: https://ocamlpro.github.io/yalo
* General Documentation: https://OCamlPro.github.io/yalo/sphinx
* API Documentation: https://OCamlPro.github.io/yalo/doc
* Sources: https://github.com/OCamlPro/yalo
* Lints Description: https://ocamlpro.github.io/yalo/lints

FAQ:

* How to configure a project ?

At the project root:

```
touch .yaloconf
yalo lint --save-config .yaloconf
```
will create a default `.yaloconf` file that you can improve.

* How to integrate in Github workflows ?

In your `.github/workflows` Yaml files, you can add the following
sections:

For permissions:
```
    permissions:
      contents: write
      security-events: write
      actions: read
```

In the workflow (typically before or after tests):
```
      - name: Run Yalo with SARIF output
        run: |
	  opam exec -- yalo lint --message-format sarif > yalo.sarif
        if: matrix.os != 'windows-latest'

      - name: Upload SARIF file
        uses: github/codeql-action/upload-sarif@v3
        if: matrix.os != 'windows-latest'
        with:
          sarif_file:  yalo.sarif
          category: yalo-lint
```

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
