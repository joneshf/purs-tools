"""
Common functions for creating actions to build PureScript programs.
"""

load(
    "//internal:providers.bzl",
    "PureScriptModuleInfo",
    "PureScriptPackageInfo",
)

def _set_rts_options(
        arguments,
        rts_options = None):
    """
    Sets the RTS options if given any.

    Args:
        arguments: An `Args` to set the RTS options on.
        rts_options: Options to pass to GHC's RTS.
            E.g. `[ "-A1G", "-N4" ]`
    """

    if rts_options != None:
        arguments.add_all(
            "+RTS",
            rts_options,
            terminate_with = "-RTS",
        )

def purs_bundle(
        ctx,
        main_module,
        out,
        index_js,
        prefix,
        deps = None,
        foreign_js = None,
        rts_options = None):
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
        rts_options: Options to pass to GHC's RTS.
            E.g. `[ "-A1G", "-N4" ]`
    """

    purs = ctx.toolchains["@joneshf_rules_purescript//purescript:toolchain_type"]

    inputs = []
    outputs = []

    arguments = ctx.actions.args()

    arguments.add("bundle")
    arguments.add("--main", main_module)
    arguments.add("--module", main_module)
    arguments.add("--output", out)

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
            arguments.add(dependency_index_js)
            inputs.append(dependency_index_js)

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
                arguments.add(dependency_foreign_js)
                inputs.append(dependency_foreign_js)

    arguments.add(index_js)
    inputs.append(index_js)

    if foreign_js != None:
        arguments.add(foreign_js)
        inputs.append(foreign_js)

    _set_rts_options(arguments, rts_options)

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

def purs_compile(
        ctx,
        output_directory,
        srcs,
        deps = None,
        ffis = None,
        ignore_warnings = False,
        rts_options = None):
    """
    Compiles a batch of PureScript modules from source.

    Args:
        ctx: Analysis context.
        output_directory: Where to place the compiled artifacts.
        srcs: The PureScript source files to be compiled.
        deps: The direct dependencies for this PureScript module.
        ffis: The optional PureScript FFI files.
        ignore_warnings: Opt-out of warnings causing a failure.
        rts_options: Options to pass to GHC's RTS.
            E.g. `[ "-A1G", "-N4" ]`
    """

    purs = ctx.toolchains["@joneshf_rules_purescript//purescript:toolchain_type"]

    inputs = depset()
    outputs = []

    arguments = ctx.actions.args()

    arguments.add("--output", output_directory.path)
    outputs.append(output_directory)

    if deps != None:
        dependencies = depset(
            direct = [dep[PureScriptPackageInfo].info.output_directory for dep in deps],
            transitive = [dep[PureScriptPackageInfo].deps for dep in deps],
        )
        arguments.add_all(
            dependencies,
            before_each = "--include",
            expand_directories = False,
        )
        inputs = depset(
            transitive = [
                dependencies,
                inputs,
            ],
        )

    if ffis != None:
        inputs = depset(
            direct = ffis,
            transitive = [
                inputs,
            ],
        )

    if ignore_warnings:
        arguments.add("--ignore-warnings")

    _set_rts_options(arguments, rts_options)

    arguments.add_all(srcs)
    inputs = depset(
        direct = srcs,
        transitive = [inputs],
    )

    ctx.actions.run(
        arguments = [
            arguments,
        ],
        executable = purs.internal.purs_compile,
        inputs = inputs,
        mnemonic = "PursCompile",
        outputs = outputs,
        progress_message = "PursCompile {name}".format(
            name = ctx.label.name,
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
        rts_options = None,
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
        rts_options: Options to pass to GHC's RTS.
            E.g. `[ "-A1G", "-N4" ]`
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

    arguments.add("--output-javascript-file", index_js)
    outputs.append(index_js)

    arguments.add("--purs-file", src)
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
            arguments.add("--input-externs-file", dependency.signature_externs)
            inputs.append(dependency.signature_externs)

    if ffi != None and foreign_js != None:
        arguments.add("--input-ffi-file", ffi)
        inputs.append(ffi)

        arguments.add("--output-ffi-file", foreign_js)
        outputs.append(foreign_js)

    if ignore_warnings:
        arguments.add("--ignore-warnings")

    if signature_externs != None:
        arguments.add("--output-signature-externs-file", signature_externs)
        outputs.append(signature_externs)

    if standard_externs != None:
        arguments.add("--output-standard-externs-file", standard_externs)
        outputs.append(standard_externs)

    _set_rts_options(arguments, rts_options)

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
