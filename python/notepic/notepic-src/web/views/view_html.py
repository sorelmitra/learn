from django.contrib import messages
from django.contrib.auth import update_session_auth_hash
from django.contrib.auth.forms import PasswordChangeForm
from django.core.files.storage import FileSystemStorage
from django.http import HttpResponseRedirect, JsonResponse
from django.shortcuts import render, redirect
from django.contrib.auth.decorators import login_required
from django.contrib.auth.forms import UserCreationForm
from django.urls import reverse_lazy
from django.views import generic
import requests
import urllib3
from ..forms import NoteForm
from ..models import Note

IMAGE_FILE_TYPES = ['png', 'jpg', 'jpeg']

def upload_image_per_request(object_type, request, image_kind, filename_noext):
    if not request.FILES:
        print(f"No <FILES> in request {request}")
        return None
    if not request.FILES[image_kind]:
        print(f"No <FILES['{image_kind}']> in request {request}")
        return None

    if len(object_type) > 0:
        object_type = f"{object_type}_"
    dir_storage = f'media/{object_type}{image_kind}/'
    myfile = request.FILES[image_kind]
    file_type = myfile.name.split('.')[-1]
    file_name = myfile.name.split('.')[0]
    file_type = file_type.lower()
    if file_type not in IMAGE_FILE_TYPES:
        return render(request, 'error_img_format.html')
    
    name = str(filename_noext) + '.' + file_type
    fs = FileSystemStorage(location=dir_storage, base_url=dir_storage)
    filename = fs.save(name, myfile)
    uploaded_file_url = fs.url(filename)
    
    print(f"uploaded_file_url={uploaded_file_url}")
    return uploaded_file_url

def index(request):
    return render(request, 'index.html')


def login(request):
    return render(request, 'login.html')

@login_required(login_url='/accounts/login/')
def notes(request):
    notes = Note.objects.all()
    return render(request, 'notes.html', {'notes': notes})

