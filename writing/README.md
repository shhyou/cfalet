Writing Practices
=====
# Directory Structure
- `*.doc[x]`: Files that are handed out.

- `*.md`: Files (for editing).

# Compilation Instruction

- How to compile `.md` files

    ```
    pandoc %.md -o %.pdf -f markdown+tex_math_single_backslash -t latex --template template.tex
    ```
