"""Dependencies needed for `ibazel`.
"""

load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_file",
)

_ibazel_linux_sha = "b43a92288df02827ebc18120065de8e9794fbfa2d19f4d79fd8d5f487efd931c"
_ibazel_macos_sha = "a450b0329c5d12673c96c6b2f14dfb8a4f6e949bd3be3d1a58085363805f688c"
_ibazel_windows_sha = "42dd267e7c28909091092d473400bb3c2a8e1990190b01a9b0f7f36bc2e86333"
_ibazel_version = "0.14.0"

def ibazel_dependencies():
    """Fetch dependencies for `ibazel`.

    Since `ibazel` is a tool we use (rather than an artifact we create),
    We want to keep its dependencies separate from ours.
    """

    http_file(
        name = "ibazel_linux",
        executable = True,
        sha256 = _ibazel_linux_sha,
        urls = [
            "https://github.com/bazelbuild/bazel-watcher/releases/download/v{ibazel_version}/ibazel_linux_amd64".format(
                ibazel_version = _ibazel_version,
            ),
        ],
    )

    http_file(
        name = "ibazel_macos",
        executable = True,
        sha256 = _ibazel_macos_sha,
        urls = [
            "https://github.com/bazelbuild/bazel-watcher/releases/download/v{ibazel_version}/ibazel_darwin_amd64".format(
                ibazel_version = _ibazel_version,
            ),
        ],
    )

    http_file(
        name = "ibazel_windows",
        executable = True,
        sha256 = _ibazel_windows_sha,
        urls = [
            "https://github.com/bazelbuild/bazel-watcher/releases/download/v{ibazel_version}/ibazel_windows_amd64.exe".format(
                ibazel_version = _ibazel_version,
            ),
        ],
    )
