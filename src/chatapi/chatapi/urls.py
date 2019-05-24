from django.urls import path
from .views import *

app_name = 'chatapi'
urlpatterns = [
	path("v1/", ViewRestPosts.as_view()),
	path("v1/<int:post_id>/", ViewRestPostById.as_view())
]
