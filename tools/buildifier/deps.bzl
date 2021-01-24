"""Dependencies needed for `buildifier`.
"""

load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_file",
)

_buildifier_linux_sha = "f9a9c082b8190b9260fce2986aeba02a25d41c00178855a1425e1ce6f1169843"
_buildifier_macos_sha = "7ecd2dac543dd2371a2261bda3eb5cbe72f54596b73f372671368f0ed5c77646"
_buildifier_windows_sha = "370a2a19ea9b131894f2999fb70281a11931bb6ede229748f0ac427a4dfa8902"
_buildifier_version = "3.5.0"

def buildifier_dependencies():
    """Fetch dependencies for `buildifier`.

    Since `buildifier` is a tool we use (rather than an artifact we create),
    We want to keep its dependencies separate from ours.
    """

    http_file(
        name = "buildifier_linux",
        executable = True,
        sha256 = _buildifier_linux_sha,
        urls = [
            "https://github.com/bazelbuild/buildtools/releases/download/{buildifier_version}/buildifier".format(
                buildifier_version = _buildifier_version,
            ),
        ],
    )

    http_file(
        name = "buildifier_macos",
        executable = True,
        sha256 = _buildifier_macos_sha,
        urls = [
            "https://github.com/bazelbuild/buildtools/releases/download/{buildifier_version}/buildifier.mac".format(
                buildifier_version = _buildifier_version,
            ),
        ],
    )

    http_file(
        name = "buildifier_windows",
        executable = True,
        sha256 = _buildifier_windows_sha,
        urls = [
            "https://github.com/bazelbuild/buildtools/releases/download/{buildifier_version}/buildifier.exe".format(
                buildifier_version = _buildifier_version,
            ),
        ],
    )
