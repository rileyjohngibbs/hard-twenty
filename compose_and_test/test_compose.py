import unittest

from compose import *

class ComposeTestCase(unittest.TestCase):

	def test_foo(self):
		foo = compose(add, mult, square)
		self.assertEqual(foo(4), 100)

	def test_bar(self):
		bar = compose(add, add, square, mult)
		self.assertEqual(bar(1), 18)

	def test_string(self):
		with self.assertRaises(TypeError):
			foo = compose(add, square)
			x = foo("hello")

	def test_non_func_composition(self):
		with self.assertRaises(TypeError):
			print "Running compose(add, 5)"
			foo = compose(add, 5)
			print "Trying to run the composition"
			x = foo(7)

	def test_no_args(self):
		with self.assertRaises(IndexError):
			foo = compose()
			foo(5)

if __name__ == '__main__':
	unittest.main()