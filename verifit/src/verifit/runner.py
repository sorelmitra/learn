from dotenv import dotenv_values

from verifit import *


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
        self._stack_number += 1
        self._create_vars(vars)
        expected, actual = self.graphql(use_token=False, check_token=True, use_expected_output=False)
        self._stack_number -= 1
        return expected, actual

    def rest(self,
             server=None,
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
             strip_regex=None,
             strip_keys=None,
             sort=None):
        self._prepare_for_test(filetype)
        if server is None:
            server = self._config["REST_SERVER"]
        command = [
            "curl", "-X", method, f'{server}{path}',
            "--header", "Content-Type: application/json; charset=UTF-8",
        ]
        if use_token:
            command += [
                "--header", f"Authorization: Bearer {self._token}",
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
        return self._run_test_command(command, strip_regex, strip_keys, sort, check_token, use_expected_output)

    def graphql(self,
                server_public=None,
                server_private=None,
                use_token=True,
                check_token=False,
                use_expected_output=True,
                strip_regex=None,
                strip_keys=None,
                sort=None):
        self._prepare_for_test("json")
        if server_public is None:
            server_public = self._config["GRAPHQL_SERVER_PUBLIC"]
        if server_private is None:
            server_private = self._config["GRAPHQL_SERVER_PRIVATE"]
        command = [
            "vitgql", "send",
            "--input-file", get_input_filename(),
            "--output-file", get_output_filename(),
        ]
        if use_token:
            command += [
                "--server", server_private,
                "--token", self._token,
            ]
        else:
            command += [
                "--server", server_public,
            ]
        return self._run_test_command(command, strip_regex, strip_keys, sort, check_token, use_expected_output)

    def websocket(self,
                  server=None,
                  use_expected_output=True,
                  ignore_messages=None,
                  strip_regex=None,
                  strip_keys=None,
                  sort=None):
        self._prepare_for_test("json")

        if server is None:
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
        ]
        if ignore_messages is not None:
            for message in ignore_messages:
                background_test_command += ["--ignore", message]
        background_test_command += [server]
        print(background_test_command)

        return run_triggered_background_test(
            background_test_command, trigger_command, use_expected_output=use_expected_output, strip_regex=strip_regex, strip_keys=strip_keys, sort=sort)

    def _run_test_command(self, command, strip_regex, strip_keys, sort, check_token, use_expected_output=True):
        set_stack_number(self._stack_number)
        output_filename = get_output_filename()
        try:
            print(f"Running command: {' '.join(command)}")
            self._expected, self._actual = run_test(command,
                                                    strip_regex=strip_regex, strip_keys=strip_keys,
                                                    sort=sort,
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

    def _create_vars(self, vars):
        self._prepare_for_test("json")
        filename_no_ext, _ = os.path.splitext(get_input_filename())
        vars_filename = f"{filename_no_ext}.vars.json"
        vars_template_filename = f"{filename_no_ext}.vars.template.json"
        with open(vars_template_filename) as f_vars_template:
            graphql_vars = f_vars_template.read()
            for key, value in vars.items():
                graphql_vars = graphql_vars.replace(f"${{{key}}}", value)
            with open(vars_filename, "wt") as f_vars:
                f_vars.write(graphql_vars)
        self._cleanup_after_test()


runner = Runner()
