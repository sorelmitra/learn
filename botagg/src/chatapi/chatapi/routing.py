# chatapi/routing.py
from django.conf.urls import url

from .consumers import *

websocket_urlpatterns = [
    url(r'^notifications/v1/$', NotificationsConsumer),
]
