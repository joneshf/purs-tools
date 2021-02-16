"""
Common functions for creating actions to build PureScript programs.
"""

load(
    "//internal:providers.bzl",
    "PureScriptModuleInfo",
)

def purs_bundle(
        ctx,
        main_module,
        out,
        index_js,
        prefix,
        deps = None,
        foreign_js = None):
    """
    Bundles a pre-compiled PureScript module.

    Args:
        ctx: Analysis context.
        main_module: The module name of the PureScript file.
        out: Where to place the bundled JavaScript file.
        index_js: The compiled JavaScript file.
            The file must be in a directory named by its module.
            E.g. `Foo.Bar/index.js`
        prefix: The prefix to next generated files under.
        deps: The direct dependencies for this PureScript module.
        foreign_js: The optional FFI files.
            The file must be in a directory named by its module.
            E.g. `Foo.Bar/foreign.js`
    """

    purs = ctx.toolchains["@joneshf_rules_purescript//purescript:toolchain_type"]

    inputs = []
    outputs = []

    arguments = ctx.actions.args()

    arguments.add("bundle")
    arguments.add("--main", main_module)
    arguments.add("--module", main_module)
    arguments.add("--output", out.path)

    index_jss = []
    foreign_jss = []

    # Collect the transitive dependencies in one directory for bundling.
    if deps != None:
        dependencies = depset(
            direct = [dep[PureScriptModuleInfo].info for dep in deps],
            transitive = [dep[PureScriptModuleInfo].deps for dep in deps],
        )
        for dependency in dependencies.to_list():
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

    index_jss.append(index_js)

    if foreign_js != None:
        foreign_jss.append(foreign_js)

    for index_js in index_jss:
        arguments.add(index_js.path)
        inputs.append(index_js)

    for foreign_js in foreign_jss:
        arguments.add(foreign_js.path)
        inputs.append(foreign_js)

    outputs.append(out)

    ctx.actions.run(
        arguments = [
            arguments,
        ],
        executable = purs.internal.purs,
        inputs = inputs,
        mnemonic = "PursBundle",
        outputs = outputs,
        progress_message = "PursBundle {main_module}".format(
            main_module = main_module,
        ),
        use_default_shell_env = True,
    )

def purs_compile_module(
        ctx,
        module_name,
        src,
        index_js,
        deps = None,
        ffi = None,
        foreign_js = None,
        ignore_warnings = False,
        signature_externs = None,
        standard_externs = None):
    """
    Compiles a single PureScript module from source.

    Args:
        ctx: Analysis context.
        module_name: The PureScript module name.
        src: The PureScript source file to be compiled.
        index_js: Where to place the compiled JavaScript file
        deps: The direct dependencies for this PureScript module.
        ffi: An optional PureScript FFI file.
            If this is supplied,
            foreign_js must also be supplied.
        foreign_js: Where to place the optional PureScript FFI file.
            If this is supplied,
            foreign_js must also be supplied.
        ignore_warnings: Opt-out of warnings causing a failure.
        signature_externs: Where to place the optional "signature" externs file.
        standard_externs: Where to place the optional "standard" externs file.
    """

    purs = ctx.toolchains["@joneshf_rules_purescript//purescript:toolchain_type"]

    if ffi != None and foreign_js == None:
        fail("Must either provide both `ffi` and `foreign_js` or neither")
    if ffi == None and foreign_js != None:
        fail("Must either provide both `ffi` and `foreign_js` or neither")

    inputs = []
    outputs = []

    arguments = ctx.actions.args()

    arguments.add("--output-javascript-file", index_js.path)
    outputs.append(index_js)

    arguments.add("--purs-file", src.path)
    inputs.append(src)

    if deps != None:
        # Since PureScript doesn't know how to work only with direct dependencies,
        # we have to flatten the depset to find all transitive dependencies.
        # This is likely to end up being O(n^2) for each purescript_library: https://docs.bazel.build/versions/master/skylark/performance.html#avoid-calling-depsetto_list.
        # The solution here is to use a compiler that understands how to work with direct dependencies only.
        # This is not a problem for bazel-related stuff to solve.
        dependencies = depset(
            direct = [dep[PureScriptModuleInfo].info for dep in deps],
            transitive = [dep[PureScriptModuleInfo].deps for dep in deps],
        )
        for dependency in dependencies.to_list():
            arguments.add("--input-externs-file", dependency.signature_externs.path)
            inputs.append(dependency.signature_externs)

    if ffi != None and foreign_js != None:
        arguments.add("--input-ffi-file", ffi.path)
        inputs.append(ffi)

        arguments.add("--output-ffi-file", foreign_js.path)
        outputs.append(foreign_js)

    if ignore_warnings:
        arguments.add("--ignore-warnings")

    if signature_externs != None:
        arguments.add("--output-signature-externs-file", signature_externs.path)
        outputs.append(signature_externs)

    if standard_externs != None:
        arguments.add("--output-standard-externs-file", standard_externs.path)
        outputs.append(standard_externs)

    ctx.actions.run(
        arguments = [
            arguments,
        ],
        executable = purs.internal.purs_compile_module,
        inputs = inputs,
        mnemonic = "PursCompileModule",
        outputs = outputs,
        progress_message = "PursCompileModule {module_name}".format(
            module_name = module_name,
        ),
        use_default_shell_env = True,
    )
