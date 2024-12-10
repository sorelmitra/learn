import os, sys

from markdownify import markdownify as md


class Markdownify:
    def __init__(self, input_filename):
        self._input_filename = input_filename
        self._converters = {
            'html': self._from_html,
            'htm': self._from_html,
        }
        self._filepath = None
        self._filename = None
        self._ext = None
    
    def convert(self):
        self._get_file_parts()
        convert_func = self._converters.get(self._ext)
        if convert_func is not None:
            convert_func()
        else:
            print(f"Error: could not find extension {self._ext} in the list of known extensions:", self._converters.keys())
            exit(1)
        
    def _get_file_parts(self):
        self._filepath, name = os.path.split(self._input_filename)
        self._filename, ext = os.path.splitext(name)
        self._ext = ext[1:]
    
    def _from_html(self):
        output_filename = os.path.join(self._filepath, f"{self._filename}.md")
        with (open(self._input_filename) as f_input):
            with (open(output_filename, "w") as f_output):
                f_output.write(md(f_input.read()))
    

input_filename = sys.argv[1]
markdownify = Markdownify(input_filename)
markdownify.convert()
