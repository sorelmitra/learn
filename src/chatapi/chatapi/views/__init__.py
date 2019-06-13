from django.http import HttpResponse

def default(request):
	return HttpResponse("Oops, you tried a wrong path!")

from .view_rest import *
