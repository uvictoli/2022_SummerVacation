from dataclasses import fields
from django.forms import ModelForm
from community.models import *

class Form(ModelForm):
    class Meta:
        model = Article
        fields = ['name', 'title', 'contents', 'url', 'email'] # 필드명은 모델.py에서 쓴 필드명과 동일해야 함