from django.urls import path, include
from django.conf.urls import url
from rest_framework import routers
from django.contrib.auth import views as auth_views

from .views import view_rest
from .views import view_html

router = routers.DefaultRouter()
router.register(r'note/v1', view_rest.NoteViewSet)

urlpatterns = [
    # ex: /polls/
    path('', view_html.index, name='index'),
    path('notes/', view_html.notes, name='notes'),
    path('api/login/', view_rest.login),
    url(r'^api/', include(router.urls)),
    url(r'^api/auth/', include('rest_framework.urls', namespace='rest_framework')),
    ]