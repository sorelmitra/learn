from django.urls import path
from .views import *

app_name = 'chatapi'
urlpatterns = [
	path("", ViewRestPosts.as_view()),
	path("<int:post_id>/", ViewRestPostById.as_view())
]
