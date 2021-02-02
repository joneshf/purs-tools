"""
Public interface for `joneshf_rules_purescript`.
"""

load(
    "@build_bazel_rules_nodejs//:index.bzl",
    "nodejs_binary",
    "nodejs_test",
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
load(
    "//internal:toolchains.bzl",
    _purescript_toolchain = "purescript_toolchain",
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

def purescript_test(name, src, module, args = None, data = None, deps = None, ignore_warnings = False, ffi = None, **kwargs):
    """
    Builds a test from PureScript source code.

    Args:
        name: A unique name for this target.
        src: Source file to compile for the main module of this test.
        module: Entry point module name.
        args: Command line arguments that Bazel passes to the target when it is executed with bazel test.
        data: Files needed by this test at runtime.
        deps: Direct dependencies for this test.
        ignore_warnings: Opt-out of warnings causing a failure.
        ffi: An optional FFI file to compile for the main module of this test.
        **kwargs: Dictionary of additional arguments.
    """

    purescript_test_name = "{name}_purs_test".format(
        name = name,
    )

    _purescript_binary(
        name = purescript_test_name,
        module = module,
        src = src,
        deps = deps,
        ignore_warnings = ignore_warnings,
        ffi = ffi,
        **kwargs
    )
    nodejs_test(
        name = name,
        args = args,
        data = data,
        entry_point = purescript_test_name,
        **kwargs
    )

purescript_toolchain = _purescript_toolchain

PureScriptModuleInfo = _PureScriptModuleInfo
