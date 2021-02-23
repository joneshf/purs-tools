"""
Rules for building PureScript.
"""

load(
    "//internal:providers.bzl",
    "PureScriptModuleInfo",
)

def _purescript_binary(ctx):
    """
    Builds an executable program from PureScript source code.

    Args:
        ctx: Analysis context.
    """

    purs = ctx.toolchains["@joneshf_rules_purescript//purescript:toolchain_type"]

    prefix = "{prefix}%".format(
        prefix = ctx.label.name,
    )

    foreign_js = None
    if ctx.file.ffi != None:
        foreign_js = ctx.actions.declare_file(
            "{prefix}/{module}/foreign.js".format(
                module = ctx.attr.module,
                prefix = prefix,
            ),
        )

    index_js = ctx.actions.declare_file(
        "{prefix}/{module}/index.js".format(
            module = ctx.attr.module,
            prefix = prefix,
        ),
    )

    purs.compile_module(
        ctx,
        deps = ctx.attr.deps,
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
    purs.bundle(
        ctx,
        deps = ctx.attr.deps,
        foreign_js = foreign_js,
        index_js = index_js,
        main_module = ctx.attr.module,
        out = executable,
        prefix = prefix,
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
        "deps": attr.label_list(
            doc = "Direct dependencies for this binary",
            providers = [
                PureScriptModuleInfo,
            ],
        ),
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
    toolchains = [
        "@joneshf_rules_purescript//purescript:toolchain_type",
    ],
)

def _purescript_library(ctx):
    """
    Compiles a PureScript module to a JavaScript file.

    Args:
        ctx: Analysis context.
    """

    purs = ctx.toolchains["@joneshf_rules_purescript//purescript:toolchain_type"]

    outputs = []

    foreign_js = None
    if ctx.file.ffi != None:
        foreign_js = ctx.actions.declare_file(
            "{module}/foreign.js".format(
                module = ctx.attr.module,
            ),
        )
        outputs.append(foreign_js)

    signature_externs_cbor = ctx.actions.declare_file(
        "{module}/signature-externs.cbor".format(
            module = ctx.attr.module,
        ),
    )
    outputs.append(signature_externs_cbor)

    standard_externs_cbor = ctx.actions.declare_file(
        "{module}/standard-externs.cbor".format(
            module = ctx.attr.module,
        ),
    )
    outputs.append(standard_externs_cbor)

    index_js = ctx.actions.declare_file(
        "{module}/index.js".format(
            module = ctx.attr.module,
        ),
    )
    outputs.append(index_js)

    purs.compile_module(
        ctx,
        deps = ctx.attr.deps,
        ffi = ctx.file.ffi,
        foreign_js = foreign_js,
        ignore_warnings = ctx.attr.ignore_warnings,
        index_js = index_js,
        module_name = ctx.attr.module,
        src = ctx.file.src,
        signature_externs = signature_externs_cbor,
        standard_externs = standard_externs_cbor,
    )

    return [
        DefaultInfo(
            files = depset([output for output in outputs]),
        ),
        PureScriptModuleInfo(
            info = struct(
                ffi_file = foreign_js,
                javascript_file = index_js,
                module_name = ctx.attr.module,
                signature_externs = signature_externs_cbor,
                standard_externs = standard_externs_cbor,
            ),
            deps = depset(
                direct = [dep[PureScriptModuleInfo].info for dep in ctx.attr.deps],
                transitive = [dep[PureScriptModuleInfo].deps for dep in ctx.attr.deps],
            ),
        ),
    ]

purescript_library = rule(
    _purescript_library,
    attrs = {
        "defines_instances": attr.bool(
            default = False,
            doc = "Whether this library defines any typeclass instances",
        ),
        "deps": attr.label_list(
            doc = "Direct dependencies for this library",
            providers = [
                PureScriptModuleInfo,
            ],
        ),
        "ffi": attr.label(
            allow_single_file = [
                ".js",
            ],
            doc = "An optional FFI file to compile for this library",
        ),
        "ignore_warnings": attr.bool(
            default = False,
            doc = "Opt-out of warnings causing a failure",
        ),
        "module": attr.string(
            doc = "The name of the module for this library",
            mandatory = True,
        ),
        "re_exports": attr.label_list(
            doc = "Re-exported modules of this library",
            providers = [
                PureScriptModuleInfo,
            ],
        ),
        "src": attr.label(
            allow_single_file = [
                ".purs",
            ],
            doc = "Source file to compile for this library",
            mandatory = True,
        ),
    },
    doc = """
Compiles a PureScript module to a JavaScript file.
""",
    toolchains = [
        "@joneshf_rules_purescript//purescript:toolchain_type",
    ],
)
