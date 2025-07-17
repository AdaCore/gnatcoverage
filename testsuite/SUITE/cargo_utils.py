import os

from SUITE.cutils import no_ext

CARGO_TEMPLATE = """[package]
name = "{pkg_name}"
edition = "2024"

[[bin]]
name = "{bin_name}"
path = "{main_path}"
"""


def cargo_for(prjid: str, main: str, src_dir: str, cargo_dir: str) -> str:
    """
    Generate a Cargo.toml manifest

    - prjid     : Name of the Cargo project
    - main      : Source simple filename which contains the main function
    - src_dir   : Directory where `main` lives
    - cargo_dir : Directory to generate the Cargo.toml
    """
    path = os.path.join(cargo_dir, "Cargo.toml")
    with open(path, "w") as f:
        f.write(
            CARGO_TEMPLATE.format(
                pkg_name=prjid,
                bin_name=no_ext(main),
                main_path=os.path.join(src_dir, main).replace("\\", "\\\\"),
            )
        )

    return path
