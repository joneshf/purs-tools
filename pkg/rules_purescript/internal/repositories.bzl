"""
Rules for setting up joneshf_rules_purescript workspace.
"""

_purs_linux_sha256 = "bed784cd3fd81039587ab531a972f225b7fa670596ecdcae17e0d56ca84eab07"
_purs_linux_url = "https://github.com/purescript/purescript/releases/download/v0.13.8/linux64.tar.gz"

_purs_macos_sha256 = "210d33ada7022569950801f101dddc013dd69274f2245aec6cab9f4704471c15"
_purs_macos_url = "https://github.com/purescript/purescript/releases/download/v0.13.8/macos.tar.gz"

_purs_compile_linux_sha256 = "c187a1f4b9c0be4c9fc79f459919653b5f859367133507047f80300e8e836cda"
_purs_compile_linux_url = "https://github.com/joneshf/purs-tools/releases/download/nightly-2021-03-15-10-01-44/purs-compile-nightly-2021-03-15-10-01-44-linux.tar.gz"

_purs_compile_macos_sha256 = "dbbcea2b9b6bf0a05589925f66c53ec8777b868d36731b5ff15bc538c3da190c"
_purs_compile_macos_url = "https://github.com/joneshf/purs-tools/releases/download/nightly-2021-03-15-10-01-44/purs-compile-nightly-2021-03-15-10-01-44-macos.tar.gz"

_purs_compile_module_linux_sha256 = "353ee208fdbe34cb6136dc03c647e27f57a8181871bc5f6cbdca3be68bfc1bee"
_purs_compile_module_linux_url = "https://github.com/joneshf/purs-tools/releases/download/nightly-2021-03-15-10-01-44/purs-compile-module-nightly-2021-03-15-10-01-44-linux.tar.gz"

_purs_compile_module_macos_sha256 = "6d118b384e47fac00e3d77e41ced048a0f3e2467244e703e11fe2b46ab8bd03c"
_purs_compile_module_macos_url = "https://github.com/joneshf/purs-tools/releases/download/nightly-2021-03-15-10-01-44/purs-compile-module-nightly-2021-03-15-10-01-44-macos.tar.gz"

_purs_module_information_linux_sha256 = "7bc6c2bfed8108e554d4edd652b98d5430aa5ac6da1ffe0b8f5d90cf88815a79"
_purs_module_information_linux_url = "https://github.com/joneshf/purs-tools/releases/download/nightly-2021-03-15-10-01-44/purs-module-information-nightly-2021-03-15-10-01-44-linux.tar.gz"

_purs_module_information_macos_sha256 = "4abe8c04c4410c1f5251a8706ba57cbc714d320911e75033546dbf5b562403b5"
_purs_module_information_macos_url = "https://github.com/joneshf/purs-tools/releases/download/nightly-2021-03-15-10-01-44/purs-module-information-nightly-2021-03-15-10-01-44-macos.tar.gz"

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
            purs_compile_sha256 = _purs_compile_linux_sha256,
            purs_compile_url = _purs_compile_linux_url,
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
            purs_compile_sha256 = _purs_compile_macos_sha256,
            purs_compile_url = _purs_compile_macos_url,
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
        purs_compile_sha256,
        purs_compile_url,
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
        purs_compile_sha256: SHA256 for purs-compile tarball.
        purs_compile_url: URL for the purs-compile tarball.
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

    ctx.report_progress("Downloading purs-compile binary")
    ctx.download_and_extract(
        sha256 = purs_compile_sha256,
        url = purs_compile_url,
    )

    ctx.report_progress("Downloading purs-compile-module binary")
    ctx.download_and_extract(
        sha256 = purs_compile_module_sha256,
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
