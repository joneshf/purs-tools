"""
Common functions for creating actions to build PureScript programs.
"""

def purs_bundle(ctx, main_module, out, index_jss, foreign_jss = None):
    """
    Bundles a pre-compiled PureScript module.

    Args:
        ctx: Analysis context.
        main_module: The module name of the PureScript file.
        out: Where to place the bundled JavaScript file.
        index_jss: The compiled JavaScript files.
            Each file must be in a directory named by its module.
            E.g. `Foo.Bar/index.js`
        foreign_jss: The optional FFI files.
            Each file must be in a directory named by its module.
            E.g. `Foo.Bar/foreign.js`
    """

    if foreign_jss == None:
        foreign_jss = []

    inputs = []
    outputs = []

    arguments = ctx.actions.args()

    arguments.add("bundle")
    arguments.add("--main", main_module)
    arguments.add("--module", main_module)
    arguments.add("--output", out.path)

    for index_js in index_jss:
        arguments.add(index_js.path)
        inputs.append(index_js)

    for foreign_js in foreign_jss:
        arguments.add(foreign_js.path)
        inputs.append(foreign_js)

    outputs.append(out)

    ctx.actions.run_shell(
        arguments = [
            arguments,
        ],
        command = "purs $@",
        inputs = inputs,
        mnemonic = "PursBundle",
        outputs = outputs,
        progress_message = "PursBundle {main_module}".format(
            main_module = main_module,
        ),
        use_default_shell_env = True,
    )

def purs_compile_module(ctx, module_name, src, index_js, ffi = None, foreign_js = None, ignore_warnings = False):
    """
    Compiles a single PureScript module from source.

    Args:
        ctx: Analysis context.
        module_name: The PureScript module name.
        src: The PureScript source file to be compiled.
        index_js: Where to place the compiled JavaScript file
        ffi: An optional PureScript FFI file.
            If this is supplied,
            foreign_js must also be supplied.
        foreign_js: Where to place the optional PureScript FFI file.
            If this is supplied,
            foreign_js must also be supplied.
        ignore_warnings: Opt-out of warnings causing a failure.
    """

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

    if ffi != None and foreign_js != None:
        arguments.add("--input-ffi-file", ffi.path)
        inputs.append(ffi)

        arguments.add("--output-ffi-file", foreign_js.path)
        outputs.append(foreign_js)

    if ignore_warnings:
        arguments.add("--ignore-warnings")

    ctx.actions.run_shell(
        arguments = [
            arguments,
        ],
        command = "purs-compile-module $@",
        inputs = inputs,
        mnemonic = "PursCompileModule",
        outputs = outputs,
        progress_message = "PursCompileModule {module_name}".format(
            module_name = module_name,
        ),
        use_default_shell_env = True,
    )
