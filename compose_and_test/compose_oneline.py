def compose(*funcs):
    return reduce(lambda x,y: lambda *args,**kwargs: y(x(*args,**kwargs)), funcs, lambda i: i)
