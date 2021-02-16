"""
Providers for PureScript.
"""

PureScriptModuleInfo = provider(
    doc = "Contains information about a PureScript module",
    fields = {
        "deps": "A depset of other PureScript modules for this module's dependencies.",
        "info": """
A struct containing information about this module.

Has the following fields:
    ffi_file: "An optional FFI module.",
    javascript_file: "The compiled JavaScript file",
    module_name: "The actual name of this PureScript module.",
    signature_externs: "The \"signature\" externs file.",
    standard_externs: "The \"standard\" externs file.",
""",
    },
)

# PureScriptToolchainInfo is a dummy provider that serves as documentation for the public interface of the ToolchainInfo provide returned by purescript_toolchain.
# Toolchains compatible with @joneshf_rules_purescript//:toolchain_type must satisfy this interface.
# No PureScriptToolchainInfo object is actually created.
PureScriptToolchainInfo = provider(
    doc = "Contains information about a PureScript toolchain",
    fields = {
        "bundle": """
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
        """,
        "compile_module": """
Compiles a single PureScript module from source.

Args:
    ctx: Analysis context.
    module_name: The PureScript module name.
    src: The PureScript source file to be compiled.
    index_js: Where to place the compiled JavaScript file
    deps: The direct dependencies for this PureScript module.
    ignore_warnings: Opt-out of warnings causing a failure.
    ffi: An optional PureScript FFI file.
        If this is supplied,
        foreign_js must also be supplied.
    foreign_js: Where to place the optional PureScript FFI file.
        If this is supplied,
        foreign_js must also be supplied.
    signature_externs: Where to place the optional "signature" externs file.
    standard_externs: Where to place the optional "standard" externs file.
        """,
    },
)
