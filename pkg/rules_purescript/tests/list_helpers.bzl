"""
Helpers for working with lists.
"""

load(
    "@bazel_skylib//lib:unittest.bzl",
    "analysistest",
    "asserts",
)

def contains(env, haystack, needle, message):
    """
    Tests if value is found in a list.

    Args:
        env: The test environment returned by analysistest.begin.
        haystack: The list to search in.
        needle: The element to search for.
        message: The failure message if the value is not found.
    """

    needle_index = haystack.index(needle)
    asserts.true(env, needle_index != -1, message)

def find_action(env, actions, mnemonic):
    """
    Attempts to find an action by its mnemonic.

    Will call analysistest.fail if the mnemonic is not found.

    Args:
        env: The test environment returned by analysistest.begin.
        actions: The list of actions to search.
        mnemonic: The mnemonic of the action to search for.

    Returns:
        The action if it exists.
    """

    for action in actions:
        if action.mnemonic == mnemonic:
            return action

    analysistest.fail(env, "Expected to generate an action: {0}".format(mnemonic))
    return None
