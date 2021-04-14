#!/bin/sh
# Convert Word docx to Markdown with pandoc, with media saved to . directory
pandoc -f docx -t markdown --wrap=none --extract-media=. -s "../GCRP CRM Technical Note post-QER.docx" -o "_technical-note.md"
# 
# Insert splitting string BREAKBREAK at every level-one header
# sed -E '/.*[A-Z][A-Z]+/ {
#     N
#     /===/ {
#         N
#         /.+\n.*==/ i\
#         BREAKBREAK
#     }
#     }' <_technical-note.md >output.tmp
# gcsplit -f crm-note -b %02d.Rmd output.tmp /BREAKBREAK/ {*} 
# rm output.tmp
# Same as above, but instead of inserting BREAKBREAK, adds # in front of Level 1 Headings and leaves as one doc
sed -E '/.*[A-Z][A-Z]+/ {
    N
    /===/ {
        N
        s/.+\n.*==/# &/
    }
    }' <_technical-note.md >output.md
# 
# Replace numbered footnotes [^1] with wildcard [^*] -- bad idea because bookdown can't take duplicate note labels
# sed -E 's/\[\^[0-9]+/\[\^\*/' <output.md >output.md
#
# Insert yaml intro from _index.md at start
cat _index.md output.md > index.Rmd
rm output.md
Rscript "_toBookdown.R"
