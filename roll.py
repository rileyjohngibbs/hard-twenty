import re

from random import randint

def roll(num,die):
	return [randint(1,die) for x in range(num)]

def roll_drop(num,drop):
	rolls = roll(num,6)
	for x in range(drop):
		rolls.remove(min(rolls))
	return rolls

def roll_4d6_stats():
	rolls = []
	for x in range(6):
		rolls.append(sum(roll_drop(4,1)))
	return rolls

def roll_single_expression(expression):
	expression = expression.strip()
	if not re.match(r'(-?[0-9]+d[0-9]+)', expression) and not re.match(r'-?[0-9]+', expression):
		raise Exception("Single roll expression must be of format NdR (e.g. 4d12) or be an integer")
	if expression[0] == '-':
		negative = True
		expression = expression[1:]
	else:
		negative = False
	if expression.isdigit():
		return int(expression)*(-1 if negative else 1)
	num, die = map(int, expression.split('d'))
	return sum(roll(num, die))*(-1 if negative else 1)

def roll_full_expression(expression):
	expressions = re.findall(r'-?(?:[0-9]+d[0-9]+|[0-9]+)', expression)
	return sum(map(roll_single_expression, expressions))
