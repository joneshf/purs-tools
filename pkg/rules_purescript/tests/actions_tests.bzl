"""
Tests for validating actions behavior.
"""

load(
    "@bazel_skylib//lib:unittest.bzl",
    "analysistest",
    "asserts",
)
load(
    "//internal:actions.bzl",
    "purs_bundle",
    "purs_compile_module",
)
load(
    ":list_helpers.bzl",
    "contains",
    "find_action",
)

def _purs_bundle_works_with_only_purescript_implementation_test(ctx):
    """
    Test to verify that compiled PureScript files generate the correct actions.
    """
    env = analysistest.begin(ctx)

    actions = analysistest.target_actions(env)
    purs_bundle_action = find_action(env, actions, "PursBundle")

    inputs = [input.basename for input in purs_bundle_action.inputs.to_list()]
    asserts.equals(env, 3, len(inputs))
    contains(env, inputs, "purs", "Expected purs to be an input")
    contains(env, inputs, "purescript-only-1.index.js", "Expected purescript-only-1.index.js to be an input")
    contains(env, inputs, "purescript-only-2.index.js", "Expected purescript-only-2.index.js to be an input")

    outputs = [output.basename for output in purs_bundle_action.outputs.to_list()]
    asserts.equals(env, 1, len(outputs))
    contains(env, outputs, "purescript-only.js", "Expected purescript-only.js to be an output")

    argv = purs_bundle_action.argv
    contains(env, argv, "--main", "Expected --main to be an argument")
    contains(env, argv, "--module", "Expected --module to be an argument")
    contains(env, argv, "--output", "Expected --output to be an argument")

    return analysistest.end(env)

def _purs_bundle_works_with_only_purescript_fake_implementation_rule(ctx):
    index_1_js = ctx.actions.declare_file("purescript-only-1.index.js")
    index_2_js = ctx.actions.declare_file("purescript-only-2.index.js")
    output_js = ctx.actions.declare_file("purescript-only.js")
    purs_bundle(
        ctx,
        index_jss = [
            index_1_js,
            index_2_js,
        ],
        main_module = "PureScriptOnly",
        out = output_js,
    )

_purs_bundle_works_with_only_purescript_fake_rule = rule(
    implementation = _purs_bundle_works_with_only_purescript_fake_implementation_rule,
    toolchains = [
        "@joneshf_rules_purescript//purescript:toolchain_type",
    ],
)

_purs_bundle_works_with_only_purescript_test = analysistest.make(
    _purs_bundle_works_with_only_purescript_implementation_test,
    expect_failure = True,
)

def _purs_bundle_works_with_purescript_and_ffi_implementation_test(ctx):
    """
    Test to verify that both compiled PureScript and FFI files generate the correct actions.
    """
    env = analysistest.begin(ctx)

    actions = analysistest.target_actions(env)
    purs_bundle_action = find_action(env, actions, "PursBundle")

    inputs = [input.basename for input in purs_bundle_action.inputs.to_list()]
    asserts.equals(env, 5, len(inputs))
    contains(env, inputs, "purs", "Expected purs to be an input")
    contains(env, inputs, "purescript-and-ffi-1.foreign.js", "Expected purescript-and-ffi-1.foreign.js to be an input")
    contains(env, inputs, "purescript-and-ffi-2.foreign.js", "Expected purescript-and-ffi-2.foreign.js to be an input")
    contains(env, inputs, "purescript-and-ffi-1.index.js", "Expected purescript-and-ffi-1.index.js to be an input")
    contains(env, inputs, "purescript-and-ffi-2.index.js", "Expected purescript-and-ffi-2.index.js to be an input")

    outputs = [output.basename for output in purs_bundle_action.outputs.to_list()]
    asserts.equals(env, 1, len(outputs))
    contains(env, outputs, "purescript-and-ffi.js", "Expected purescript-and-ffi.js to be an output")

    argv = purs_bundle_action.argv
    contains(env, argv, "--main", "Expected --main to be an argument")
    contains(env, argv, "--module", "Expected --module to be an argument")
    contains(env, argv, "--output", "Expected --output to be an argument")

    return analysistest.end(env)

def _purs_bundle_works_with_purescript_and_ffi_fake_implementation_rule(ctx):
    foreign_1_js = ctx.actions.declare_file("purescript-and-ffi-1.foreign.js")
    foreign_2_js = ctx.actions.declare_file("purescript-and-ffi-2.foreign.js")
    index_1_js = ctx.actions.declare_file("purescript-and-ffi-1.index.js")
    index_2_js = ctx.actions.declare_file("purescript-and-ffi-2.index.js")
    output_js = ctx.actions.declare_file("purescript-and-ffi.js")
    purs_bundle(
        ctx,
        foreign_jss = [
            foreign_1_js,
            foreign_2_js,
        ],
        index_jss = [
            index_1_js,
            index_2_js,
        ],
        main_module = "PureScriptAndFFI",
        out = output_js,
    )

_purs_bundle_works_with_purescript_and_ffi_fake_rule = rule(
    implementation = _purs_bundle_works_with_purescript_and_ffi_fake_implementation_rule,
    toolchains = [
        "@joneshf_rules_purescript//purescript:toolchain_type",
    ],
)

_purs_bundle_works_with_purescript_and_ffi_test = analysistest.make(
    _purs_bundle_works_with_purescript_and_ffi_implementation_test,
    expect_failure = True,
)

def _purs_compile_module_fails_with_incorrect_ffi_implementation_test(ctx):
    """
    Test to verify that both the input and output FFI files must be given together.
    """
    env = analysistest.begin(ctx)

    asserts.expect_failure(env, "Must either provide both `ffi` and `foreign_js` or neither")

    return analysistest.end(env)

def _purs_compile_module_fails_with_only_ffi_fake_implementation_rule(ctx):
    ffi = ctx.actions.declare_file("Foo.js")
    index_js = ctx.actions.declare_file("index.js")
    src = ctx.actions.declare_file("Foo.purs")
    purs_compile_module(
        ctx,
        ffi = ffi,
        index_js = index_js,
        module_name = "Foo",
        src = src,
    )

_purs_compile_module_fails_with_only_ffi_fake_rule = rule(
    implementation = _purs_compile_module_fails_with_only_ffi_fake_implementation_rule,
    toolchains = [
        "@joneshf_rules_purescript//purescript:toolchain_type",
    ],
)

_purs_compile_module_fails_with_only_ffi_test = analysistest.make(
    _purs_compile_module_fails_with_incorrect_ffi_implementation_test,
    expect_failure = True,
)

def _purs_compile_module_fails_with_only_foreign_js_fake_implementation_rule(ctx):
    foreign_js = ctx.actions.declare_file("foreign.js")
    index_js = ctx.actions.declare_file("index.js")
    src = ctx.actions.declare_file("Foo.purs")
    purs_compile_module(
        ctx,
        foreign_js = foreign_js,
        index_js = index_js,
        module_name = "Foo",
        src = src,
    )

_purs_compile_module_fails_with_only_foreign_js_fake_rule = rule(
    implementation = _purs_compile_module_fails_with_only_foreign_js_fake_implementation_rule,
    toolchains = [
        "@joneshf_rules_purescript//purescript:toolchain_type",
    ],
)

_purs_compile_module_fails_with_only_foreign_js_test = analysistest.make(
    _purs_compile_module_fails_with_incorrect_ffi_implementation_test,
    expect_failure = True,
)

def _purs_compile_module_works_with_only_purescript_implementation_test(ctx):
    """
    Test to verify that only PureScript files generate the correct actions.
    """
    env = analysistest.begin(ctx)

    actions = analysistest.target_actions(env)
    purs_compile_module_action = find_action(env, actions, "PursCompileModule")

    inputs = [input.basename for input in purs_compile_module_action.inputs.to_list()]
    asserts.equals(env, 2, len(inputs))
    contains(env, inputs, "purs-compile-module", "Expected purs-compile-module to be an input")
    contains(env, inputs, "PureScriptOnly.purs", "Expected PureScriptOnly.purs to be an input")

    outputs = [output.basename for output in purs_compile_module_action.outputs.to_list()]
    asserts.equals(env, 1, len(outputs))
    contains(env, outputs, "purescript-only.index.js", "Expected purescript-only.index.js to be an output")

    argv = purs_compile_module_action.argv
    contains(env, argv, "--output-javascript-file", "Expected --output-javascript-file to be an argument")
    contains(env, argv, "--purs-file", "Expected --purs-file to be an argument")

    return analysistest.end(env)

def _purs_compile_module_works_with_only_purescript_fake_implementation_rule(ctx):
    index_js = ctx.actions.declare_file("purescript-only.index.js")
    src = ctx.actions.declare_file("PureScriptOnly.purs")
    purs_compile_module(
        ctx,
        index_js = index_js,
        module_name = "PureScriptOnly",
        src = src,
    )

_purs_compile_module_works_with_only_purescript_fake_rule = rule(
    implementation = _purs_compile_module_works_with_only_purescript_fake_implementation_rule,
    toolchains = [
        "@joneshf_rules_purescript//purescript:toolchain_type",
    ],
)

_purs_compile_module_works_with_only_purescript_test = analysistest.make(
    _purs_compile_module_works_with_only_purescript_implementation_test,
    expect_failure = True,
)

def _purs_compile_module_works_with_purescript_and_ffi_implementation_test(ctx):
    """
    Test to verify that both PureScript and FFI files generate the correct actions.
    """
    env = analysistest.begin(ctx)

    actions = analysistest.target_actions(env)
    purs_compile_module_action = find_action(env, actions, "PursCompileModule")

    inputs = [input.basename for input in purs_compile_module_action.inputs.to_list()]
    asserts.equals(env, 3, len(inputs))
    contains(env, inputs, "purs-compile-module", "Expected purs-compile-module to be an input")
    contains(env, inputs, "PureScriptAndFFI.purs", "Expected PureScriptAndFFI.purs to be an input")
    contains(env, inputs, "PureScriptAndFFI.js", "Expected PureScriptAndFFI.js to be an input")

    outputs = [output.basename for output in purs_compile_module_action.outputs.to_list()]
    asserts.equals(env, 2, len(outputs))
    contains(env, outputs, "purescript-and-ffi.index.js", "Expected purescript-and-ffi.index.js to be an output")
    contains(env, outputs, "purescript-and-ffi.foreign.js", "Expected purescript-and-ffi.foreign.js to be an output")

    argv = purs_compile_module_action.argv
    contains(env, argv, "--input-ffi-file", "Expected --input-ffi-file to be an argument")
    contains(env, argv, "--output-ffi-file", "Expected --output-ffi-file to be an argument")
    contains(env, argv, "--output-javascript-file", "Expected --output-javascript-file to be an argument")
    contains(env, argv, "--purs-file", "Expected --purs-file to be an argument")

    return analysistest.end(env)

def _purs_compile_module_works_with_purescript_and_ffi_fake_implementation_rule(ctx):
    ffi = ctx.actions.declare_file("PureScriptAndFFI.js")
    foreign_js = ctx.actions.declare_file("purescript-and-ffi.foreign.js")
    index_js = ctx.actions.declare_file("purescript-and-ffi.index.js")
    src = ctx.actions.declare_file("PureScriptAndFFI.purs")
    purs_compile_module(
        ctx,
        ffi = ffi,
        foreign_js = foreign_js,
        index_js = index_js,
        module_name = "PureScriptAndFFI",
        src = src,
    )

_purs_compile_module_works_with_purescript_and_ffi_fake_rule = rule(
    implementation = _purs_compile_module_works_with_purescript_and_ffi_fake_implementation_rule,
    toolchains = [
        "@joneshf_rules_purescript//purescript:toolchain_type",
    ],
)

_purs_compile_module_works_with_purescript_and_ffi_test = analysistest.make(
    _purs_compile_module_works_with_purescript_and_ffi_implementation_test,
    expect_failure = True,
)

def purs_bundle_tests_suite(name):
    """
    A suite of tests around purs_bundle.

    Args:
        name: A unique name for this target.
    """

    _purs_bundle_works_with_only_purescript_test(
        name = "purs_bundle_works_with_only_purescript_test",
        target_under_test = ":purs_bundle_works_with_only_purescript_fake_target",
    )
    _purs_bundle_works_with_only_purescript_fake_rule(
        name = "purs_bundle_works_with_only_purescript_fake_target",
        tags = [
            "manual",
        ],
    )

    _purs_bundle_works_with_purescript_and_ffi_test(
        name = "purs_bundle_works_with_purescript_and_ffi_test",
        target_under_test = ":purs_bundle_works_with_purescript_and_ffi_fake_target",
    )
    _purs_bundle_works_with_purescript_and_ffi_fake_rule(
        name = "purs_bundle_works_with_purescript_and_ffi_fake_target",
        tags = [
            "manual",
        ],
    )

def purs_compile_module_tests_suite(name):
    """
    A suite of tests around purs_compile_module.

    Args:
        name: A unique name for this target.
    """

    _purs_compile_module_fails_with_only_ffi_test(
        name = "purs_compile_module_fails_with_only_ffi_test",
        target_under_test = ":purs_compile_module_fails_with_only_ffi_fake_target",
    )
    _purs_compile_module_fails_with_only_ffi_fake_rule(
        name = "purs_compile_module_fails_with_only_ffi_fake_target",
        tags = [
            "manual",
        ],
    )

    _purs_compile_module_fails_with_only_foreign_js_test(
        name = "purs_compile_module_fails_with_only_foreign_js_test",
        target_under_test = ":purs_compile_module_fails_with_only_foreign_js_fake_target",
    )
    _purs_compile_module_fails_with_only_foreign_js_fake_rule(
        name = "purs_compile_module_fails_with_only_foreign_js_fake_target",
        tags = [
            "manual",
        ],
    )

    _purs_compile_module_works_with_only_purescript_test(
        name = "purs_compile_module_works_with_only_purescript_test",
        target_under_test = ":purs_compile_module_works_with_only_purescript_fake_target",
    )
    _purs_compile_module_works_with_only_purescript_fake_rule(
        name = "purs_compile_module_works_with_only_purescript_fake_target",
        tags = [
            "manual",
        ],
    )

    _purs_compile_module_works_with_purescript_and_ffi_test(
        name = "purs_compile_module_works_with_purescript_and_ffi_test",
        target_under_test = ":purs_compile_module_works_with_purescript_and_ffi_fake_target",
    )
    _purs_compile_module_works_with_purescript_and_ffi_fake_rule(
        name = "purs_compile_module_works_with_purescript_and_ffi_fake_target",
        tags = [
            "manual",
        ],
    )
