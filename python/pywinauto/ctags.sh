#!/bin/sh
files=ctags.files
echo "Generating $files..."
find . -iname '*.py' > $files && find ../pywinauto/ -iname '*.py' >> $files
echo "Generating tags for files in $files..."
ctags -L $files
echo "Done."
