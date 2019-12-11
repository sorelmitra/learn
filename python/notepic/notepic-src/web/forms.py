from django import forms
from .models import Note

class NoteForm(forms.Form):
    title = forms.CharField(max_length=200)
    description = forms.CharField(widget=forms.Textarea, required=False)
    thumbnail = forms.FileField(required=False)

