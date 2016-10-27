import math

def generate_all_rolls(expression):
	expression.strip()
	num, die = map(int, expression.split('d'))
	rolls = [[]]
	die_faces = map(lambda x: [x],range(1,die+1))
	for n in range(num):
		rolls = cross_product(rolls,die_faces)
	return rolls
		
def cross_product(x,y):
	new_list = []
	for j in y:
		new_list.extend([i+j for i in x])
	return new_list

def drop_lowest(roll, drop):
	for x in range(drop):
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

def calculate_n_highest(num, die, values):
	if len(values) > num:
		raise Exception("The list of values is greater than the number of dice to be rolled.")
	all_rolls = brute_drop(num, die, num-len(values))
	positive_results = filter(lambda x: sorted(x)==sorted(values), all_rolls)
	return float(len(positive_results))/len(all_rolls)

def calculate_drop(num, die, drop):
	bits = []
	for k in range(1,die+1):
		#print (float(k)/die)**drop
		bits.append( (float(k)/die)**drop * combination(3,2) * mean_range(k,die) * (num-drop) )
	return sum(bits)

print calculate_n_highest(4,6,[3,5])
print calculate_n_highest(4,6,[5])
print calculate_n_highest(4,6,[3,5])/calculate_n_highest(4,6,[5])
