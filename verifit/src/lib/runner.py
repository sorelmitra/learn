from verifit import *
from config_tool import *
from graphql_tool import GraphQLTool
from websockets_tool import WebsocketsTool


class Runner:
    def __init__(self):
        self._stack_number = 3
        self._had_exception = None
        self._actual = None
        self._expected = None
        self._old_stack_number = None
        self._old_filetype = None
        self._token = ''

    def cli(self,
            command,
            variables=None,
            use_expected_output=True,
            strip_regex=None,
            strip_keys=None,
            sort=None):
        self._prepare_for_test("json")
        if variables is not None:
            self._create_vars(get_input_filename(), variables)
        return self._run_test_command(command, strip_regex, strip_keys, sort, False, use_expected_output)

    def login(self, path, username, password):
        return self.rest(path=path, method="POST",
                         use_input_file=False, use_expected_output=False,
                         input_data_raw={"username": username,
                                         "password": password},
                         use_token=False, check_token=True)

    def login_graphql(self, input_filename, variables):
        self._stack_number += 1
        self._create_vars(input_filename, variables)
        expected, actual = self.graphql(use_token=False, check_token=True, use_expected_output=False)
        self._stack_number -= 1
        return expected, actual

    def rest(self,
             server=None,
             path='',
             method="GET",
             variables=None,
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
            server = Config.value["REST_SERVER"]
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
        if variables is not None:
            self._create_vars(get_input_filename(), variables)
        return self._run_test_command(command, strip_regex, strip_keys, sort, check_token, use_expected_output)

    def graphql(self,
                server_public=None,
                server_private=None,
                variables=None,
                use_token=True,
                check_token=False,
                use_expected_output=True,
                strip_regex=None,
                strip_keys=None,
                sort=None):
        self._prepare_for_test("json")
        gql_tool = GraphQLTool(
            server_public=server_public,
            server_private=server_private,
            input_filename=get_input_filename(),
            use_token=use_token,
            token=self._token)
        if variables is not None:
            self._create_vars(get_input_filename(), variables)
        return self._run_test_command(gql_tool.prepare_query_and_command(),
                                      strip_regex, strip_keys, sort,
                                      check_token, use_expected_output)

    def websocket(self,
                  server=None,
                  variables=None,
                  use_expected_output=True,
                  ignore_messages=None,
                  strip_regex=None,
                  strip_keys=None,
                  sort=None):
        self._prepare_for_test("json")
        if variables is not None:
            self._create_vars(get_input_filename(), variables)
        ws_tool = WebsocketsTool(server=server,
                                 input_filename=get_input_filename(),
                                 output_filename=get_output_filename(),
                                 ignore_list=ignore_messages)
        return run_test(func=ws_tool.run,
                        use_expected_output=use_expected_output,
                        strip_regex=strip_regex,
                        strip_keys=strip_keys,
                        sort=sort)

    def _run_test_command(self, command, strip_regex, strip_keys, sort, check_token, use_expected_output=True):
        set_stack_number(self._stack_number)
        output_filename = get_output_filename()
        try:
            print(f"Running command: {' '.join(command)}")
            self._expected, self._actual = run_test(command=command,
                                                    use_expected_output=use_expected_output,
                                                    strip_regex=strip_regex,
                                                    strip_keys=strip_keys,
                                                    sort=sort)
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

    def _create_vars(self, input_filename, variables):
        filename_no_ext, _ = os.path.splitext(input_filename)
        vars_filename = f"{filename_no_ext}.vars.json"
        vars_template_filename = f"{filename_no_ext}.vars.template.json"
        with open(vars_template_filename) as f_vars_template:
            graphql_vars = f_vars_template.read()
            for key, value in variables.items():
                graphql_vars = graphql_vars.replace(f"${{{key}}}", value)
            with open(vars_filename, "wt") as f_vars:
                f_vars.write(graphql_vars)
        self._cleanup_after_test()


runner = Runner()
