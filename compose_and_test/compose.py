def compose(*funcs):
	if len(funcs) == 1:
		def composition(*args, **kwargs):
			return funcs[-1](*args, **kwargs)
	else:
		def composition(*args, **kwargs):
			return funcs[-1](compose(*funcs[:-1])(*args, **kwargs))
	return composition

def add(x): return x+1
def mult(x): return x*2
def square(x): return x**2
