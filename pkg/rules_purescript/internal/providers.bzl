"""
Providers for PureScript.
"""

PureScriptModuleInfo = provider(
    doc = "Contains information about a PureScript module",
    fields = {
        "deps": "A depset of other PureScript modules for this module's dependencies.",
        "info": """
A struct containing information about this module.

Has the following fields:
    ffi_file: "An optional FFI module.",
    javascript_file: "The compiled JavaScript file",
    module_name: "The actual name of this PureScript module.",
    signature_externs: "The \"signature\" externs file.",
""",
    },
)
