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

    externs_files = []
    foreign_jss = []
    index_jss = []

    # Since PureScript doesn't know how to work only with immediate dependencies,
    # we have to flatten the depset to find all transitive dependencies.
    # This is likely to end up being O(n^2) for each purescript_binary: https://docs.bazel.build/versions/master/skylark/performance.html#avoid-calling-depsetto_list.
    # The solution here is to use a compiler that understands how to work with direct dependencies only.
    # This is not a problem for bazel-related stuff to solve.
    dependencies = depset(
        direct = [dep[PureScriptModuleInfo].info for dep in ctx.attr.deps],
        transitive = [dep[PureScriptModuleInfo].deps for dep in ctx.attr.deps],
    )
    for dependency in dependencies.to_list():
        externs_files.append(dependency.signature_externs)

        dependency_index_js = ctx.actions.declare_file(
            "{prefix}/{module}/index.js".format(
                module = dependency.module_name,
                prefix = prefix,
            ),
        )
        ctx.actions.symlink(
            output = dependency_index_js,
            target_file = dependency.javascript_file,
        )
        index_jss.append(dependency_index_js)

        if dependency.ffi_file != None:
            dependency_foreign_js = ctx.actions.declare_file(
                "{prefix}/{module}/foreign.js".format(
                    module = dependency.module_name,
                    prefix = prefix,
                ),
            )
            ctx.actions.symlink(
                output = dependency_foreign_js,
                target_file = dependency.ffi_file,
            )
            foreign_jss.append(dependency_foreign_js)

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

    purs.compile_module(
        ctx,
        externs_files = externs_files,
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

    externs_files = []
    outputs = []

    # Since PureScript doesn't know how to work only with direct dependencies,
    # we have to flatten the depset to find all transitive dependencies.
    # This is likely to end up being O(n^2) for each purescript_library: https://docs.bazel.build/versions/master/skylark/performance.html#avoid-calling-depsetto_list.
    # The solution here is to use a compiler that understands how to work with direct dependencies only.
    # This is not a problem for bazel-related stuff to solve.
    dependencies = depset(
        direct = [dep[PureScriptModuleInfo].info for dep in ctx.attr.deps],
        transitive = [dep[PureScriptModuleInfo].deps for dep in ctx.attr.deps],
    )
    for dependency in dependencies.to_list():
        externs_files.append(dependency.signature_externs)

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
        externs_files = externs_files,
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
