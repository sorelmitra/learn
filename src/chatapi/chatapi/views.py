from django.shortcuts import render

# Create your views here.

from django.http import HttpResponse

def index(request):
	return HttpResponse("Hi World from BotAgg Chat API!")

def default(request):
	return HttpResponse("Oops, you tried a wrong path!")
