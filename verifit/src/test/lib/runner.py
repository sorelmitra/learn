from dotenv import dotenv_values

from verifit import *
from verifit import get_input_filename, get_output_filename


class Runner:
    def __init__(self):
        self._had_exception = None
        self._actual = None
        self._expected = None
        self._old_stack_number = None
        self._old_filetype = None
        self._ENV = os.environ['ENV']
        self._config = {
            **dotenv_values(f".{self._ENV}.env")
        }
        self._token = ''

    def rest(self,
             path='',
             method="GET",
             filetype=None,
             use_token=True,
             use_input_file=True,
             use_output_file=True,
             retrieve_headers=False,
             sort=None):
        self._prepare_for_test(filetype)
        command = [
            "curl", "-X", method, f'{self._config["REST_SERVER"]}{path}',
            "--header", "Content-Type: application/json; charset=UTF-8",
        ]
        if use_token:
            command += [
                "--header", f"Authentication: Bearer {self._token}",
            ]
        if use_input_file:
            command += [
                "--data-binary", f"@{get_input_filename()}",
            ]
        if use_output_file:
            command += [
                "--output", f"{get_output_filename()}",
            ]
        if retrieve_headers:
            command += [
                "--include",
            ]
        return self._run_test_command(command, sort)

    def _run_test_command(self, command, sort):
        try:
            set_stack_number(3)
            print(f"Running command: {' '.join(command)}")
            self._expected, self._actual = run_test(command, strip=[r'\nDate:[^\n]*'], sort=sort)
        except Exception as e:
            _had_exception = True
            self._actual = e
            print(e)
        finally:
            self._cleanup_after_test()
            if self._had_exception:
                raise self._actual
            return self._expected, self._actual

    def _prepare_for_test(self, filetype):
        self._old_filetype = get_data_file_type()
        if filetype is not None:
            set_data_file_type(filetype)
        self._old_stack_number = get_stack_number()
        set_stack_number(2)
        self._expected, self._actual = "the test was run", "something happened"
        self._had_exception = False

    def _cleanup_after_test(self):
        set_stack_number(self._old_stack_number)
        set_data_file_type(self._old_filetype)


runner = Runner()
