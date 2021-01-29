"""
Rules for building PureScript.
"""

load(
    "//internal:actions.bzl",
    "purs_bundle",
    "purs_compile_module",
)

def _purescript_binary(ctx):
    """
    Builds an executable program from PureScript source code.

    Args:
        ctx: Analysis context.
    """

    prefix = "{prefix}%".format(
        prefix = ctx.label.name,
    )

    foreign_jss = []
    index_jss = []

    foreign_js = None
    if ctx.file.ffi != None:
        foreign_js = ctx.actions.declare_file(
            "{prefix}/{module}/foreign.js".format(
                module = ctx.attr.module,
                prefix = prefix,
            ),
        )
        foreign_jss.append(foreign_js)

    index_js = ctx.actions.declare_file(
        "{prefix}/{module}/index.js".format(
            module = ctx.attr.module,
            prefix = prefix,
        ),
    )
    index_jss.append(index_js)

    purs_compile_module(
        ctx,
        ffi = ctx.file.ffi,
        foreign_js = foreign_js,
        ignore_warnings = ctx.attr.ignore_warnings,
        index_js = index_js,
        module_name = ctx.attr.module,
        src = ctx.file.src,
    )

    executable = ctx.actions.declare_file(
        "{prefix}/{name}.js".format(
            name = ctx.label.name,
            prefix = prefix,
        ),
    )
    purs_bundle(
        ctx,
        foreign_jss = foreign_jss,
        index_jss = index_jss,
        main_module = ctx.attr.module,
        out = executable,
    )

    return [
        DefaultInfo(
            files = depset(
                [
                    executable,
                ],
            ),
            executable = executable,
        ),
    ]

purescript_binary = rule(
    _purescript_binary,
    attrs = {
        "ffi": attr.label(
            allow_single_file = [
                ".js",
            ],
            doc = "FFI file to compile for the main module of this binary",
        ),
        "ignore_warnings": attr.bool(
            default = False,
            doc = "Opt-out of warnings causing a failure",
        ),
        "module": attr.string(
            doc = "Entry point module name",
            mandatory = True,
        ),
        "src": attr.label(
            allow_single_file = [
                ".purs",
            ],
            doc = "Source file to compile for the main module of this binary",
            mandatory = True,
        ),
    },
    doc = """
Builds an executable program from PureScript source code.

This will bundle up all of the JavaScript into a single file.
""",
    executable = True,
)
