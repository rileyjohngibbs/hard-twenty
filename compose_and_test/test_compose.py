import unittest

from compose import *
from compose_oneline import compose as compose_oneline

class SimpleTestCase(unittest.TestCase):

	def test_compose(self):
		c = compose(add, mult, square)
		self.assertEqual(144, c(5))

	def test_oneline(self):
		c = compose_oneline(add, mult, square)
		self.assertEqual(144, c(5))

class StringTestCase(unittest.TestCase):

	def test_compose(self):
		with self.assertRaises(TypeError):
			c = compose(add, mult, square)
			c("hello")

	def test_oneline(self):
		with self.assertRaises(TypeError):
			c = compose_oneline(add, mult, square)
			c("hello")

class NonFunctionTestCase(unittest.TestCase):

	def test_compose(self):
		with self.assertRaises(TypeError):
			c = compose(add, 5)
			c(5)

	def test_oneline(self):
		with self.assertRaises(TypeError):
			c = compose_oneline(add, 5)
			c(5)

class NoArgsTestCase(unittest.TestCase):

	def test_compose(self):
		with self.assertRaises(IndexError):
			c = compose()
			c(5)

	def test_oneline(self):
		c = compose_oneline()
		self.assertEqual(5, c(5)) 

if __name__ == '__main__':
	unittest.main()
