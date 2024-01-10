from typing import Dict
from scripts.common_conf import VAR_REPLACEMENTS


def get_var_replacements(do_level: str, doc_id: str) -> Dict[str, str]:
    """Return var replacements for the given project"""

    return VAR_REPLACEMENTS | {"doc_id": doc_id, "dolevel": do_level}
