import json

from django.views import View
from django.http import HttpResponse
from django.http import HttpRequest

from .models import *

def default(request):
	return HttpResponse("Oops, you tried a wrong path!")

def errorResponse(e):
	response = {
		'success': False,
		'post': {},
		'reason': f'{e}'
	}
	return HttpResponse(json.dumps(response))

class ViewRestPosts(View):
	def get(self, request, *args, **kwargs):
		unicodeBody = request.body.decode('utf-8')
		print(f"Body is: {unicodeBody}")
		print(f"Args are: {args}")
		print(f"Keyword args are: {kwargs}")
		posts = []
		try:
			for post in Post.objects.all():
				posts.append({
					'id': post.id,
					'body': post.text
				})
			response = {
				'success': True,
				'posts': posts,
				'reason': 'messages retrieved'
			}
		except Exception as e:
			return errorResponse(e)
		return HttpResponse(json.dumps(response))

	def post(self, request, *args, **kwargs):
		unicodeBody = request.body.decode('utf-8')
		try:
			data = json.loads(unicodeBody)
		except Exception as e:
			return errorResponse(e)
		
		print(f"Data is: {data}")
		print(f"Args are: {args}")
		print(f"Keyword args are: {kwargs}")
		post = Post(text=data['body'])
		try:
			post.save()
		except Exception as e:
			return errorResponse(e)
		response = {
			'success': True,
			'post': {
				'id': post.id,
				'body': post.text
			},
			'reason': 'message posted successfully'
		}
		return HttpResponse(json.dumps(response))

	def delete(self, request, *args, **kwargs):
		unicodeBody = request.body.decode('utf-8')
		print(f"Body is: {unicodeBody}")
		print(f"Args are: {args}")
		print(f"Keyword args are: {kwargs}")
		try:
			Post.objects.all().delete()
		except Exception as e:
			return errorResponse(e)
		response = {
			'success': True,
			'posts': [],
			'reason': 'all messages deleted'
		}
		return HttpResponse(json.dumps(response))


class ViewRestPostById(View):
	def get(self, request, *args, **kwargs):
		unicodeBody = request.body.decode('utf-8')
		print(f"Body is: {unicodeBody}")
		print(f"Args are: {args}")
		print(f"Keyword args are: {kwargs}")
		posts = []
		try:
			post_id = kwargs['post_id']
			post = Post.objects.get(id=post_id)
			response = {
				'success': True,
				'post': {
					'id': post.id,
					'body': post.text
				},
				'reason': 'message retrieved'
			}
		except Exception as e:
			return errorResponse(e)
		return HttpResponse(json.dumps(response))

