import generate_doc

# Locate the output as the other sphinx-based components do, facilitating the
# toplevel driver for qmat generation. Not an unreasonable name anyway.

generate_doc.generate_html('PLANS', 'build/html')
