from django.views import View
from django.http import HttpResponse

from .models import *

class ViewPost(View):
	def get(self, request, *args, **kwargs):
		print(f"Args are: {args}")
		print(f"Keyword args are: {kwargs}")
		post_id = kwargs['post_id']
		post = Post.objects.get(id=post_id)
		return HttpResponse(f"Post with ID {post_id}: '{post.text}'")

