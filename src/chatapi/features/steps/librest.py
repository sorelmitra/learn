import os, string, requests, sys, json

def launchRestMethodCall(method, url, data = None, login = None, params = None, headers = None):
    print(f"Launching REST {method}:")
    print(f"      URL: <{url}>")
    print(f"     data: {data}")
    print(f"    login: {login}")
    print(f"   params: {params}")
    print(f"  headers: {headers}")
    
    if method == 'POST':
        response = requests.post(url, auth=login, params=params, data=data, headers=headers, verify=False)
    elif method == 'PUT':
        response = requests.put(url, auth=login, params=params, data=data, headers=headers, verify=False)
    elif method == 'GET':
        response = requests.get(url, auth=login, params=params, data=data, headers=headers, verify=False)
    elif method == 'DELETE':
        response = requests.delete(url, auth=login, params=params, data=data, headers=headers, verify=False)
    else:
        raise RuntimeError("Unimplemented method " + method)
    
    print(f"Response: Status {response.status_code} <<{response.text}>>")
    try:
        json = response.json()
    except ValueError:
        json = {'status': response.status_code, 'text': response.text}
    return (response.status_code, json)

