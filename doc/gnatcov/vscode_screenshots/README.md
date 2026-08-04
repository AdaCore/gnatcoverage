# Screenshots for the VS Code external annotations section

Images used by the section `ext_annot_vscode` in `../external_annotations.rst`.

| File | Shows |
|---|---|
| `annotations-editor.png` | A source file with its annotations displayed: the inline badge naming each annotation kind, the tinted background of an `Exempt_Region`, and the annotation list in the sidebar. |
| `annotations-tree.png` | The **GNATcoverage Annotations** view alone, with annotations grouped by source file, including a stale one. Cropped from `annotations-editor.png`. |

## Re-capturing them

Both come from one screenshot of a workspace whose
`ada.externalAnnotations.file` setting points at an annotation file holding a
few annotations of different kinds, so that the distinct colours per category
are visible. A stale annotation is worth keeping in the picture, since it is the
case the editor cannot display.

The crop for `annotations-tree.png` is:

```python
from PIL import Image
Image.open("annotations-editor.png").crop((40, 440, 352, 730)).save(
    "annotations-tree.png", optimize=True)
```

Note that capturing these needs a real VS Code session: the extension only
activates in a trusted workspace, and workspace trust is stored per browser
profile, so an automated headless session shows the workbench without any
annotation in it.
