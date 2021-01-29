"""
Tests for validating rules behavior.
"""

load(
    "@bazel_skylib//lib:unittest.bzl",
    "analysistest",
    "asserts",
)
load(
    "//internal:rules.bzl",
    "purescript_binary",
)
load(
    ":list_helpers.bzl",
    "contains",
    "find_action",
)

def _purescript_binary_works_with_only_purescript_implementation_test(ctx):
    """
    Test to verify that compiled PureScript files generate the correct actions.
    """
    env = analysistest.begin(ctx)

    actions = analysistest.target_actions(env)
    purs_compile_module_action = find_action(env, actions, "PursCompileModule")

    inputs = [input.basename for input in purs_compile_module_action.inputs.to_list()]
    asserts.equals(env, 1, len(inputs))
    contains(env, inputs, "PureScriptOnly.purs", "Expected PureScriptOnly.purs to be an input")

    outputs = [output.basename for output in purs_compile_module_action.outputs.to_list()]
    asserts.equals(env, 1, len(outputs))
    contains(env, outputs, "index.js", "Expected index.js to be an output")

    argv = purs_compile_module_action.argv
    contains(env, argv, "--output-javascript-file", "Expected --output-javascript-file to be an argument")
    contains(env, argv, "--purs-file", "Expected --purs-file to be an argument")

    purs_bundle_action = find_action(env, actions, "PursBundle")

    inputs = [input.basename for input in purs_bundle_action.inputs.to_list()]
    asserts.equals(env, 1, len(inputs))
    contains(env, inputs, "index.js", "Expected index.js to be an input")

    outputs = [output.basename for output in purs_bundle_action.outputs.to_list()]
    asserts.equals(env, 1, len(outputs))
    contains(env, outputs, "purescript_binary_works_with_only_purescript_fake_target.js", "Expected purescript_binary_works_with_only_purescript_fake_target.js to be an output")

    argv = purs_bundle_action.argv
    contains(env, argv, "--main", "Expected --main to be an argument")
    contains(env, argv, "--module", "Expected --module to be an argument")
    contains(env, argv, "--output", "Expected --output to be an argument")

    return analysistest.end(env)

_purescript_binary_works_with_only_purescript_test = analysistest.make(
    _purescript_binary_works_with_only_purescript_implementation_test,
)

def _purescript_binary_works_with_purescript_and_ffi_implementation_test(ctx):
    """
    Test to verify that both compiled PureScript and FFI files generate the correct actions.
    """
    env = analysistest.begin(ctx)

    actions = analysistest.target_actions(env)
    purs_compile_module_action = find_action(env, actions, "PursCompileModule")

    inputs = [input.basename for input in purs_compile_module_action.inputs.to_list()]
    asserts.equals(env, 2, len(inputs))
    contains(env, inputs, "PureScriptAndFFI.js", "Expected PureScriptAndFFI.js to be an input")
    contains(env, inputs, "PureScriptAndFFI.purs", "Expected PureScriptAndFFI.purs to be an input")

    outputs = [output.basename for output in purs_compile_module_action.outputs.to_list()]
    asserts.equals(env, 2, len(outputs))
    contains(env, outputs, "foreign.js", "Expected foreign.js to be an output")
    contains(env, outputs, "index.js", "Expected index.js to be an output")

    argv = purs_compile_module_action.argv
    contains(env, argv, "--input-ffi-file", "Expected --input-ffi-file to be an argument")
    contains(env, argv, "--output-ffi-file", "Expected --output-ffi-file to be an argument")
    contains(env, argv, "--output-javascript-file", "Expected --output-javascript-file to be an argument")
    contains(env, argv, "--purs-file", "Expected --purs-file to be an argument")

    purs_bundle_action = find_action(env, actions, "PursBundle")

    inputs = [input.basename for input in purs_bundle_action.inputs.to_list()]
    asserts.equals(env, 2, len(inputs))
    contains(env, inputs, "foreign.js", "Expected foreign.js to be an input")
    contains(env, inputs, "index.js", "Expected index.js to be an input")

    outputs = [output.basename for output in purs_bundle_action.outputs.to_list()]
    asserts.equals(env, 1, len(outputs))
    contains(env, outputs, "purescript_binary_works_with_purescript_and_ffi_fake_target.js", "Expected purescript_binary_works_with_purescript_and_ffi_fake_target.js to be an output")

    argv = purs_bundle_action.argv
    contains(env, argv, "--main", "Expected --main to be an argument")
    contains(env, argv, "--module", "Expected --module to be an argument")
    contains(env, argv, "--output", "Expected --output to be an argument")

    return analysistest.end(env)

_purescript_binary_works_with_purescript_and_ffi_test = analysistest.make(
    _purescript_binary_works_with_purescript_and_ffi_implementation_test,
)

def purescript_binary_tests_suite(name):
    """
    A suite of tests around purescript_binary.

    Args:
        name: A unique name for this target.
    """

    _purescript_binary_works_with_only_purescript_test(
        name = "purescript_binary_works_with_only_purescript_test",
        target_under_test = ":purescript_binary_works_with_only_purescript_fake_target",
    )
    purescript_binary(
        name = "purescript_binary_works_with_only_purescript_fake_target",
        module = "PureScriptOnly",
        src = "PureScriptOnly.purs",
        tags = [
            "manual",
        ],
    )

    _purescript_binary_works_with_purescript_and_ffi_test(
        name = "purescript_binary_works_with_purescript_and_ffi_test",
        target_under_test = ":purescript_binary_works_with_purescript_and_ffi_fake_target",
    )
    purescript_binary(
        name = "purescript_binary_works_with_purescript_and_ffi_fake_target",
        ffi = "PureScriptAndFFI.js",
        module = "PureScriptAndFFI",
        src = "PureScriptAndFFI.purs",
        tags = [
            "manual",
        ],
    )
