from django.urls import path
from . import views
from .ViewPost import *

app_name = 'chatapi'
urlpatterns = [
	path("", views.index, name="index"),
	path("<int:post_id>/", ViewPost.as_view())
]
