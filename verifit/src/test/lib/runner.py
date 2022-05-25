from dotenv import dotenv_values

from verifit import *
from verifit import get_input_filename, get_output_filename


class Runner:
    def __init__(self):
        self._stack_number = 3
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

    def login(self, path, username, password):
        return self.rest(path=path, method="POST",
                         use_input_file=False, use_expected_output=False,
                         input_data_raw={"username": username,
                                         "password": password},
                         use_token=False, check_token=True)

    def login_graphql(self, vars):
        self._create_vars(get_input_filename(), vars)
        return self.graphql(use_token=False, check_token=True, use_expected_output=False)

    def rest(self,
             path='',
             method="GET",
             filetype=None,
             use_token=True,
             check_token=False,
             use_input_file=True,
             input_data_raw=None,
             use_output_file=True,
             use_expected_output=True,
             retrieve_headers=False,
             follow_redirects=False,
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
        if input_data_raw is not None:
            command += [
                "--data-binary", f"{json.dumps(input_data_raw)}",
            ]
        if use_output_file:
            command += [
                "--output", f"{get_output_filename()}",
            ]
        if retrieve_headers:
            command += [
                "--include",
            ]
        if follow_redirects:
            command += [
                "--location",
            ]
        return self._run_test_command(command, sort, check_token, use_expected_output)

    def graphql(self, use_token=True, check_token=False, use_expected_output=True, sort=None):
        self._prepare_for_test("json")
        command = [
            "vitgql", "send",
            "--input-file", get_input_filename(),
            "--output-file", get_output_filename(),
        ]
        if use_token:
            command += [
                "--server", self._config["GRAPHQL_SERVER_PRIVATE"],
                "--token", self._token,
            ]
        else:
            command += [
                "--server", self._config["GRAPHQL_SERVER_PUBLIC"],
            ]
        return self._run_test_command(command, sort, check_token, use_expected_output)

    def websocket(self, use_expected_output=True, sort=None):
        self._prepare_for_test("json")

        server = runner._config['WEBSOCKETS_SERVER_URL']

        trigger_command = [
            "vitwss", "send",
            "--input-file", get_input_filename(),
            server
        ]

        background_test_command = [
            "vitwss", "receive",
            "--packets-to-receive", "1",
            "--wait-ms", "10000",
            "--output-file", get_output_filename(),
            server
        ]

        return run_triggered_background_test(
            background_test_command, trigger_command, use_expected_output=use_expected_output, sort=sort)

    def _run_test_command(self, command, sort, check_token, use_expected_output=True):
        set_stack_number(self._stack_number)
        output_filename = get_output_filename()
        try:
            print(f"Running command: {' '.join(command)}")
            self._expected, self._actual = run_test(command,
                                                    strip=[r'\nDate:[^\n]*'], sort=sort,
                                                    use_expected_output=use_expected_output)
        except Exception as e:
            _had_exception = True
            self._actual = e
            print(e)
        finally:
            self._cleanup_after_test()
            if self._had_exception:
                raise self._actual
            if check_token:
                res = json.loads(self._actual)
                self._expected = "a token was returned (don't care about its value)"
                try:
                    self._token = res['accessToken']
                    self._actual = self._expected
                except KeyError:
                    try:
                        self._token = res['data']['login']['accessToken']
                        self._actual = self._expected
                    except KeyError:
                        self._actual = f"no token was returned"
                if not use_expected_output:
                    os.unlink(output_filename)
            return self._expected, self._actual

    def _prepare_for_test(self, filetype):
        self._old_filetype = get_data_file_type()
        if filetype is not None:
            set_data_file_type(filetype)
        self._old_stack_number = get_stack_number()
        set_stack_number(self._stack_number - 1)
        self._expected, self._actual = "the test was run", "something happened"
        self._had_exception = False

    def _cleanup_after_test(self):
        set_stack_number(self._old_stack_number)
        set_data_file_type(self._old_filetype)

    @staticmethod
    def _create_vars(input_filename, vars):
        filename_no_ext, _ = os.path.splitext(input_filename)
        vars_filename = f"{filename_no_ext}.vars.json"
        vars_template_filename = f"{filename_no_ext}.vars.template.json"
        with open(vars_template_filename) as f_vars_template:
            graphql_vars = f_vars_template.read()
            for key, value in vars.items():
                graphql_vars = graphql_vars.replace(f"${{{key}}}", value)
            with open(vars_filename, "wt") as f_vars:
                f_vars.write(graphql_vars)


runner = Runner()
