from runner import *


def test_hello():
    expected, actual = runner.cli(["cp", "-v", get_input_filename(), get_output_filename()])
    assert actual == expected
