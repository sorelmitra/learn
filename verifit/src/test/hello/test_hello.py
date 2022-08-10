from verifit import *


def test_hello():
    expected, actual = run_test(["cp", "-v", get_input_filename(), get_output_filename()])
    assert actual == expected
