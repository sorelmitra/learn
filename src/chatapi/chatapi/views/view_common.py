import json

from django.http import HttpResponse

def errorResponse(e):
	response = {
		'success': False,
		'post': {},
		'reason': f'{e}'
	}
	return HttpResponse(json.dumps(response))

