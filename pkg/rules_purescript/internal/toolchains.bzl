"""
Rules for setting up the PureScript toolchain.
"""

load(
    ":actions.bzl",
    "purs_bundle",
    "purs_compile_module",
)

def _purescript_toolchain(ctx):
    return [
        platform_common.ToolchainInfo(
            bundle = purs_bundle,
            compile_module = purs_compile_module,
            internal = struct(
                purs_compile_module = ctx.file.purs_compile_module,
                purs = ctx.file.purs,
            ),
        ),
    ]

purescript_toolchain = rule(
    _purescript_toolchain,
    attrs = {
        "purs": attr.label(
            allow_single_file = True,
            doc = "The official PureScript compiler",
            mandatory = True,
        ),
        "purs_compile_module": attr.label(
            allow_single_file = True,
            doc = "The unofficial single-module PureScript compiler",
            mandatory = True,
        ),
    },
    doc = """
Gathers functions and file lists needed for the PureScript toolchain.
""",
)
