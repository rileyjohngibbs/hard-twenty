import math

PRIMES = [2]

def is_prime_opt(n):
	if n < PRIMES[-1] and n in PRIMES:
		return True
	if n == 1:
		return False
	mx = int(math.sqrt(n))
	for prime in PRIMES:
		# Don't try primes greater than the square root
		if prime > mx:
			break
		# Check if primes divide candidate
		if n%prime == 0:
			return False
	# Add in a check whether there are other primes between PRIMES[-1] and candidate
	PRIMES.append(n)
	return True

def next_prime_opt(n):
	m = n+1
	while not is_prime_opt(m):
		m += 1
	return m

# Test next_prime_opt
some_primes = [2,3,5,7,11,13,17,19,23,29]
x = 2
while x < some_primes[-1]:
	x = next_prime_opt(x)
if PRIMES != some_primes:
	print 'Function is_prime_opt did not generate correctly { p | p <= 29 }. Expected %s, got %s.' % (some_primes, PRIMES)
PRIMES = [2]

# Put the primes in a file on your desktop. It will take a while.
with open('~/Desktop/primes_opt.txt','w') as primesfile:
     while len(str(x)) < 10:
             primesfile.write('%s\n'%x)
             x = next_prime_opt(x)
