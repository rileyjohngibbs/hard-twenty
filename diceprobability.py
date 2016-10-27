import math
import time
from pprint import pprint

def generate_all_rolls(expression):
	expression.strip()
	num, die = map(int, expression.split('d'))
	rolls = [[]]
	die_faces = map(lambda x: [x],range(1,die+1))
	for n in range(num):
		rolls = cross_product(rolls,die_faces)
	return rolls
		
def cross_product(x,y):
	x = x[:]
	y = y[:]
	new_list = []
	for j in y:
		new_list.extend([i+j for i in x])
	return new_list

def drop_lowest(roll, drop_count=1):
	for x in range(drop_count):
		roll.remove(min(roll))
	return roll

def brute_drop(num, die, drop, sum_up=False):
	rolls = map(lambda x: drop_lowest(x,drop), generate_all_rolls("%sd%s" % (num, die)))
	return map(sum, rolls) if sum_up else rolls

def mean(values):
	return float(sum(values))/len(values)

def combination(n, r):
	return math.factorial(n)/(math.factorial(r)*math.factorial(n-r))

def sum_range(a, b):
	return (b+a)*(b-a+1)*0.5

def mean_range(a, b):
	return (a+b)*0.5

def calculate_highest(num, die):
	return [[x, (float(x)/die)**num - (float(x-1)/die)**num] for x in range(1,die+1)]

def brute_calculate_n_highest(num, die, values):
	if len(values) > num:
		raise Exception("The list of values is greater than the number of dice to be rolled.")
	all_rolls = brute_drop(num, die, num-len(values))
	positive_results = filter(lambda x: sorted(x)==sorted(values), all_rolls)
	return float(len(positive_results))/len(all_rolls)

def calculate_n_highest(num, die, values):
	value_counts = {value:values.count(value) for value in set(values)}
	positive_results = 1
	dice_counted = 0
	for i,value in enumerate(sorted(value_counts.keys(), reverse=True)):
		if i < len(value_counts.keys())-1:
			positive_results *= combination(num - dice_counted, value_counts[value])
			dice_counted += value_counts[value]
		else:
			positive_results *= sum(combination(num - dice_counted, num - dice_counted - k)*(value-1)**k for k in range(num - dice_counted - value_counts[value] + 1))
	return float(positive_results)/die**num

def calculate_n_highest_sum(num, die, drop_count, value):
	'''Calculates the probability that, when rolling num-d-die,
	and dropping drop_count lowest dice, the sum is value.'''
	combo_set = all_sum_combos(num - drop_count, die, value, dedupe=True)
	return sum([calculate_n_highest(num, die, combo) for combo in combo_set])

def all_sum_combos(num, die, value, dedupe=False):
	combo_set = [[]]
	die_faces = map(lambda x: [x],range(1,die+1))
	for n in range(num):
		combo_set = filter(lambda x: sum(x) < value, combo_set)
		combo_set = cross_product(combo_set, die_faces)
	combo_set = filter(lambda x: sum(x) == value, combo_set)
	if dedupe:
		combo_set = map(sorted, combo_set)
		for combo in combo_set[:]:
			while combo_set.count(combo) > 1:
				combo_set.remove(combo)
	return combo_set

def calculate_drop(num, die, drop, value):
	desired_combos = []
	combo_length = num - drop


test_values = [
	(4, 6, [5,3]),
	(4, 6, [5,5]),
	(4, 6, [6,1]),
	(4, 6, [6,6,6]),
	#(8, 6, [6,1])
]

'''for test_value in test_values:
	print brute_calculate_n_highest(*test_value)
	print calculate_n_highest(*test_value)

probabilities = {x:calculate_n_highest(3,20,[x]) for x in range(1,21)}
for value in probabilities:
	print value, probabilities[value]
print sum(probabilities[value]*value for value in probabilities)'''


'''combo_sets = [all_sum_combos(3,6,x) for x in range(3,19)]
for i,combo_set in enumerate(combo_sets):
	combo_set = map(sorted, combo_set)
	combo_sets[i] = combo_set
	for combo in combo_set[:]:
		while combo_set.count(combo) > 1:
			combo_set.remove(combo)
for combo_set in combo_sets:
	print sum(combo_set[0]), sum([calculate_n_highest(4,6,combo) for combo in combo_set]), calculate_n_highest_sum(4,6,1,sum(combo))

all_rolls = map(drop_lowest, generate_all_rolls("4d6"))
for x in range(3,19):
	print x, float(len(filter(lambda r: sum(r)==x, all_rolls)))/6**4'''

for n in range(3):
	for x in range(3,19):
		print x, calculate_n_highest_sum(3+n,6,n,x)
	print '-'*10
	print sum([x*calculate_n_highest_sum(3+n,6,n,x) for x in range(3,19)])
	print '='*10

num = 8
die = 6
drop_count = 2

start_time = time.time()
rolls = brute_drop(num, die, drop_count, True)
for value in sorted(set(rolls)):
	print value, float(rolls.count(value))/len(rolls)
print time.time() - start_time
print '='*10

start_time = time.time()
for x in range(num - drop_count, (num - drop_count) * die + 1):
	print x, calculate_n_highest_sum(num, die, drop_count, x)
print time.time() - start_time