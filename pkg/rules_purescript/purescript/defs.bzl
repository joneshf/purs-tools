"""
Public interface for `joneshf_rules_purescript`.
"""

load(
    "@build_bazel_rules_nodejs//:index.bzl",
    "nodejs_binary",
)
load(
    "//internal:providers.bzl",
    _PureScriptModuleInfo = "PureScriptModuleInfo",
)
load(
    "//internal:rules.bzl",
    _purescript_binary = "purescript_binary",
    _purescript_library = "purescript_library",
)

def purescript_binary(name, src, module, deps = None, ffi = None, ignore_warnings = False, **kwargs):
    """
    Builds an executable program from PureScript source code.

    Args:
        name: A unique name for this target.
        src: Source file to compile for the main module of this binary.
        module: Entry point module name.
        deps: Direct dependencies for this binary.
        ffi: An optional FFI file to compile for the main module of this binary.
        ignore_warnings: Opt-out of warnings causing a failure.
        **kwargs: Dictionary of additional arguments.
    """

    purescript_binary_name = "{name}_purs".format(
        name = name,
    )

    _purescript_binary(
        name = purescript_binary_name,
        module = module,
        src = src,
        deps = deps,
        ffi = ffi,
        ignore_warnings = ignore_warnings,
        **kwargs
    )
    nodejs_binary(
        name = name,
        entry_point = purescript_binary_name,
        **kwargs
    )

purescript_library = _purescript_library

PureScriptModuleInfo = _PureScriptModuleInfo
