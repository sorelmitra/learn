import json

from django.views import View
from django.http import HttpResponse
from django.http import HttpRequest

from .view_common import *
from ..models import *
from ..utils.postnotif import *

class ViewRestPosts(View):

	def __init__(self, **kwargs):
		self.register = getPostNotificationsRegister()
		self.commandFactory = PostNotifCommandsFactory(self.register, self)

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

		name = 'unknown'
		try:
			name = data['name']
		except KeyError as e:
			return errorResponse("Missing client name, post refused. Please add a 'name' field")

		post = Post(text=data['body'])
		try:
			post.save()
		except Exception as e:
			return errorResponse(e)
		respPost = {
			'name': name,
			'id': post.id,
			'body': post.text
		}
		response = {
			'success': True,
			'post': respPost,
			'reason': 'message posted successfully'
		}
		self.notifyPost(respPost)
		return HttpResponse(json.dumps(response))
	
	def notifyPost(self, post):
		content = {
			'command': 'notify-all',
			'post': post
		}
		command = self.commandFactory.create(content)
		command.execute()

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

