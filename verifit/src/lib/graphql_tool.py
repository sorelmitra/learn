import json

from config_tool import *


class GraphQLTool:
    def __init__(self,
                 server_public=None,
                 server_private=None,
                 input_filename=None,
                 use_token=False,
                 token=None):
        self.LOG = print

        if use_token:
            self._server = server_private
            if self._server is None:
                self._server = Config.value["GRAPHQL_SERVER_PUBLIC"]
        else:
            self._server = server_public
        if self._server is None:
            self._server = Config.value["GRAPHQL_SERVER_PRIVATE"]
        
        self._filename_graphql_opname = input_filename
        self._fill_in_filenames()
        self._use_token = use_token
        self._token = token

    def prepare_query_and_command(self):
        self._prepare_graphql_query()
        launch_command = [
            "curl",
            "-X", "POST",
            "-H" "Content-Type: application/json",
            "--compressed",
            "--data", f"@{self._filename_graphql_request}",
            "-o", self._filename_graphql_response,
        ]
        if self._use_token:
            launch_command = launch_command + ["-H", f"Authorization: {self._token}", ]
        launch_command = launch_command + [self._server]
        self.LOG(f"GraphQL command: {launch_command}")
        return launch_command

    @staticmethod
    def _escape_for_json(str1):
        str1 = str1.replace('"', '\\"').replace('\n', '\\n').replace('\\', '\\\\')
        return str1

    def _opname_path(self, filename):
        return os.path.join(os.path.dirname(self._filename_graphql_opname), filename)

    def _fill_in_filenames(self):
        filename_no_ext, _ = os.path.splitext(os.path.basename(self._filename_graphql_opname))
        self._filename_graphql_query = self._opname_path(f'{filename_no_ext}.graphql')
        self._filename_graphql_variables = self._opname_path(f'{filename_no_ext}.vars.json')
        self._filename_graphql_request = self._opname_path(f'{filename_no_ext}.req.json')
        self._filename_graphql_response = self._opname_path(f'{filename_no_ext}-answer.json')

    def _prepare_graphql_query(self):
        self.LOG(
            f'Generating GraphQL request to {self._filename_graphql_request} '
            f'based on {self._filename_graphql_opname}, '
            f'{self._filename_graphql_query}, {self._filename_graphql_variables}')
        with open(self._filename_graphql_opname) as f_opname:
            body = json.load(f_opname)
            with open(self._filename_graphql_query) as f_query:
                query = f_query.read()
                with open(self._filename_graphql_variables) as f_vars:
                    variables = f_vars.read()
        body['query'] = query
        body['variables'] = variables

        with open(self._filename_graphql_request, "wt") as f_out:
            json.dump(body, f_out)
