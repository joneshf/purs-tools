# rules_purescript

[PureScript][] rules for [bazel][]

# Getting started

```starlark
load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive",
)

http_archive(
    name = "joneshf_rules_purescript",
    sha256 = "<SHA256>",
    strip_prefix = "rules_purescript-1.0.0/pkg/rules_purescript",
    urls = [
        "https://github.com/joneshf/purs-tools/releases/download/rules_purescript-1.0.0/rules_purescript-1.0.0.tar.gz",
    ],
)

load(
    "@joneshf_rules_purescript//purescript:deps.bzl",
    "purescript_rules_dependencies",
    "purescript_register_toolchains",
)

purescript_rules_dependencies()

purescript_register_toolchains()
```

[bazel]: https://www.bazel.build/
[purescript]: https://www.purescript.org/
