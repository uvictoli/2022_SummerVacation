from django.shortcuts import render
from community.forms import Form
from community.models import Article

# Create your views here.

#urls.py에 넣으 write 함수 정의
def write(request): #urls에서 사용자로부터 요청이 들어오면
    if request.method == 'POST':
        form = Form(request.POST)
        if form.is_valid():
            form.save() # DB에 필드값 저장
    else:
        form = Form() # 그냥 화면에 출력
            
    return render(request, 'write.html', {'form' : form})
    
def list(request):
    articleList = Article.objects.all()
    return render(request, "list.html", {'articleList' : articleList})

def view(request, num = "1"):
    article = Article.objects.get(id = num)
    return render(request, "view.html", {'article' : article})