"""
External dependences that `joneshf_rules_purescript` depends on.
"""

load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive",
)
load(
    "@bazel_tools//tools/build_defs/repo:utils.bzl",
    "maybe",
)
load(
    "//internal:repositories.bzl",
    "purescript_download",
)

def rules_purescript_dependencies():
    """
    Declares external repositories that `joneshf_rules_purescript` depends on.

    This function should be loaded and called from WORKSPACE files.
    """

    maybe(
        http_archive,
        "bazel_skylib",
        sha256 = "1c531376ac7e5a180e0237938a2536de0c54d93f5c278634818e0efc952dd56c",
        urls = [
            "https://github.com/bazelbuild/bazel-skylib/releases/download/1.0.3/bazel-skylib-1.0.3.tar.gz",
            "https://mirror.bazel.build/github.com/bazelbuild/bazel-skylib/releases/download/1.0.3/bazel-skylib-1.0.3.tar.gz",
        ],
    )

    maybe(
        http_archive,
        "build_bazel_rules_nodejs",
        sha256 = "84b1d11b1f3bda68c24d992dc6e830bca9db8fa12276f2ca7fcb7761c893976b",
        urls = [
            "https://github.com/bazelbuild/rules_nodejs/releases/download/3.0.0-rc.1/rules_nodejs-3.0.0-rc.1.tar.gz",
        ],
    )

# buildifier: disable=unnamed-macro
def rules_purescript_toolchains():
    """
    Registers the toolchains that `joneshf_rules_purescript` depends on.

    This function should be loaded and called from WORKSPACE files.
    """

    purescript_download(
        name = "joneshf_rules_purescript_purs_linux",
        platform = "linux",
    )
    native.register_toolchains("@joneshf_rules_purescript_purs_linux//:toolchain")

    purescript_download(
        name = "joneshf_rules_purescript_purs_macos",
        platform = "macos",
    )
    native.register_toolchains("@joneshf_rules_purescript_purs_macos//:toolchain")
