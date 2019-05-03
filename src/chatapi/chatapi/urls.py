from django.urls import path
from . import views

app_name = 'chatapi'
urlpatterns = [
	path("", views.index, name="index")
]