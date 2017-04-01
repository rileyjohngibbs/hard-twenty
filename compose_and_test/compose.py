# import unittest
# from test import test_support

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

'''bar = compose(add, mult, square)

print bar(4) # Should print 100 to screen

class MyTestCase1(unittest.TestCase):

	def test_foo(self):
		foo = compose(add, mult, square)
		assert foo(4) == 10, "Fuuuuuck"

	def test_bar(self):
		bar = compose(add, add, square, mult)
		assert bar(1) == 10, "Triple fuuuuuuck"

test_support.run_unittest(MyTestCase1)'''