load(
    "@joneshf_rules_purescript//purescript:defs.bzl",
    "purescript_toolchain",
)

exports_files(
    [
        "purs-compile-module",
        "purs-module-information",
        "purs",
    ],
    visibility = [
        "//visibility:public",
    ],
)

filegroup(
    name = "tools",
    srcs = [
        "purs-compile-module",
        "purs-module-information",
        "purs",
    ],
    visibility = [
        "//visibility:public",
    ],
)

purescript_toolchain(
    name = "toolchain_impl",
    purs = "purs",
    purs_compile_module = "purs-compile-module",
)

toolchain(
    name = "toolchain",
    exec_compatible_with = [
        "{exec_compatible_with}",
    ],
    target_compatible_with = [
        "{target_compatible_with}",
    ],
    toolchain = ":toolchain_impl",
    toolchain_type = "@joneshf_rules_purescript//purescript:toolchain_type",
)
