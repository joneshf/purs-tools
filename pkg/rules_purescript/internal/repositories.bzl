"""
Rules for setting up joneshf_rules_purescript workspace.
"""

_purs_linux_sha256 = "bed784cd3fd81039587ab531a972f225b7fa670596ecdcae17e0d56ca84eab07"
_purs_linux_url = "https://github.com/purescript/purescript/releases/download/v0.13.8/linux64.tar.gz"

_purs_macos_sha256 = "210d33ada7022569950801f101dddc013dd69274f2245aec6cab9f4704471c15"
_purs_macos_url = "https://github.com/purescript/purescript/releases/download/v0.13.8/macos.tar.gz"

_purs_compile_module_linux_sha256 = "2c6b7d4f1e8e1c737bf899ef83d320bde8bdaaf5e5d537e4939f1a582f90b481"
_purs_compile_module_linux_url = "https://bintray.com/api/ui/download/joneshf/generic/purs-compile-module-1.0.0-rc2-linux.tar.gz"

_purs_compile_module_macos_sha256 = "665333dd29c69ad400f5382f1399ffa643f94d7d8ba818ba53ba7c7ae0d2de98"
_purs_compile_module_macos_url = "https://bintray.com/api/ui/download/joneshf/generic/purs-compile-module-1.0.0-rc2-macos.tar.gz"

_purs_module_information_linux_sha256 = "7f2d36595908d8b976c7db9f1a872d5bec627e3a929421039e3cd5e576435094"
_purs_module_information_linux_url = "https://github.com/joneshf/purs-tools/releases/download/nightly-2021-02-16-02-55-07/purs-module-information-nightly-2021-02-16-02-55-07-linux.tar.gz"

_purs_module_information_macos_sha256 = "5c4f64a958b92e37ce7b7d78b26252c45f5e3737ffd95e0b04a410f8e924ee33"
_purs_module_information_macos_url = "https://github.com/joneshf/purs-tools/releases/download/nightly-2021-02-16-02-55-07/purs-module-information-nightly-2021-02-16-02-55-07-macOS.tar.gz"

def _purescript_download(ctx):
    """
    Downloads PureScript binaries and installs a build file.

    Args:
        ctx: Repository context.
    """

    if ctx.attr.platform == "linux":
        _purescript_download_binary(
            ctx,
            purs_sha256 = _purs_linux_sha256,
            purs_url = _purs_linux_url,
            purs_compile_module_sha256 = _purs_compile_module_linux_sha256,
            purs_compile_module_url = _purs_compile_module_linux_url,
            purs_module_information_sha256 = _purs_module_information_linux_sha256,
            purs_module_information_url = _purs_module_information_linux_url,
            exec_compatible_with = "@platforms//os:linux",
            target_compatible_with = "@platforms//os:linux",
        )
    elif ctx.attr.platform == "macos":
        _purescript_download_binary(
            ctx,
            purs_sha256 = _purs_macos_sha256,
            purs_url = _purs_macos_url,
            purs_compile_module_sha256 = _purs_compile_module_macos_sha256,
            purs_compile_module_url = _purs_compile_module_macos_url,
            purs_module_information_sha256 = _purs_module_information_macos_sha256,
            purs_module_information_url = _purs_module_information_macos_url,
            exec_compatible_with = "@platforms//os:macos",
            target_compatible_with = "@platforms//os:macos",
        )
    else:
        fail(
            "{platform} is not currently supported".format(
                platform = ctx.attr.platform,
            ),
        )

def _purescript_download_binary(
        ctx,
        purs_sha256,
        purs_url,
        purs_compile_module_sha256,
        purs_compile_module_url,
        purs_module_information_sha256,
        purs_module_information_url,
        exec_compatible_with,
        target_compatible_with):
    """
    Downloads PureScript binaries for a given platform and installs a build file.

    Args:
        ctx: Repository context.
        purs_sha256: SHA256 for Purs tarball.
        purs_url: URL for the Purs tarball.
        purs_compile_module_sha256: SHA256 for purs-compile-module tarball.
        purs_compile_module_url: URL for the purs-compile-module tarball.
        purs_module_information_sha256: SHA256 for purs-module-information tarball.
        purs_module_information_url: URL for the purs-module-information tarball.
        exec_compatible_with: A constraint value that must be satisfied by an execution platform.
        target_compatible_with: A constraint value that must be satisfied by the target platform.
    """

    ctx.report_progress("Downloading purs binary")
    ctx.download_and_extract(
        sha256 = purs_sha256,
        stripPrefix = "purescript",
        url = purs_url,
    )

    ctx.report_progress("Downloading purs-compile-module binary")
    ctx.download_and_extract(
        sha256 = purs_compile_module_sha256,
        stripPrefix = "bin",
        url = purs_compile_module_url,
    )

    ctx.report_progress("Downloading purs-module-information binary")
    ctx.download_and_extract(
        sha256 = purs_module_information_sha256,
        url = purs_module_information_url,
    )

    substitutions = {
        "{exec_compatible_with}": exec_compatible_with,
        "{target_compatible_with}": target_compatible_with,
    }

    ctx.template(
        "BUILD.bazel",
        ctx.attr._build_template,
        substitutions = substitutions,
    )

purescript_download = repository_rule(
    _purescript_download,
    attrs = {
        "platform": attr.string(
            doc = "Host operating system for PureScript binaries",
            mandatory = True,
            values = [
                "linux",
                "macos",
            ],
        ),
        "_build_template": attr.label(
            default = "@joneshf_rules_purescript//internal:BUILD.purescript_download.bazel.tpl",
        ),
    },
    doc = """
Downloads PureScript binaries and installs a build file.
""",
)
