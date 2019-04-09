from django.urls import path
from . import views

app_name = 'webchatapp'
urlpatterns = [
	path("", views.index, name="index")
]