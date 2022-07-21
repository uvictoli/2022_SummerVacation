from django.db import models
# 모델 = 우리가 필요한 데이터
# 모델은 기본적으로 클래스로 생성, 모델을 상속

class Article(models.Model):
    name = models.CharField(max_length=50)
    title = models.CharField(max_length=50)
    contents = models.TextField()
    url = models.URLField()
    email = models.EmailField()
    cdate = models.DateTimeField(auto_now_add = True)
    
    