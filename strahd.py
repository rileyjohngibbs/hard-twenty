import random
import re
import sys
import roll

def pick_random(source_list):
	return source_list[random.randint(0,len(source_list)-1)]

class StrahdNPC(object):

	first_names = {
		"female": [
			"Alana", "Clavdia", "Danya", "Dezdrelda", "Diavola", "Dorina",
			"Drasha", "Drilvia", "Elisabeta", "Fatima", "Grilsha", "Isabella",
			"Ivana", "Jarzinka", "Kala", "Katerina", "Kereza", "Korina",
			"Lavinia", "Magda", "Marta", "Mathilda", "Minodora", "Mirabel",
			"Miruna", "Nimira", "Nyanka", "Olivenka", "Ruxandra", "Sorina",
			"Tereska", "Valentina", "Vasha", "Victoria", "Wensencia", "Zondra"
		],
		"male": [
			"Alek", "Andrej", "Anton", "Balthazar", "Bogan", "Boris",
			"Dargos", "Darzin", "Dragomir", "Emeric", "Falkon", "Frederich",
			"Franz", "Gargosh", "Gorek", "Grygori", "Hans", "Harkus", "Ivan",
			"Jirko", "Kobal", "Korga", "Krystofor", "Lazlo", "Livius",
			"Marek", "Miroslav", "Nikolaj", "Nimir", "Oleg", "Radovan",
			"Radu", "Seraz", "Sergei", "Stefan", "Tural", "Valentin",
			"Vasily", "Vladislav", "Waltar", "Yesper", "Zsolt"
		]
	}

	family_names = {
		"female": "a",
		"male": "ich",
		"base": [
			"Alastroi", "Antonov>m/f", "Barthos", "Belasco", "Cantemir",
			"Dragov>m/f", "Diavolov", "Diminski", "Dilisnya", "Drazkoi",
			"Garvinski", "Grejenko", "Groza", "Grygorov>m/f", "Ivanov>m/f",
			"Janek", "Karushkin", "Konstantinov>m/f", "Krezkov>m/f", "Kryski",
			"Lansten", "Lazarescu", "Lukresh", "Lipsiege", "Martikov>f",
			"Mironov>m/fn", "Moldovar", "Nikolov>m/f", "Nimirov>m/f",
			"Oronov>m/f", "Petrov>m/fn", "Polensky", "Radov>m/f",
			"Rilsky", "Stefanov>m/f", "Strazni", "Swilov>m/f", "Taltos",
			"Targolov>f", "Tyminski", "Ulbrek", "Ulrich", "Vadu",
			"Voltanescu", "Zalenski", "Zalken"
		]
	}

	soul_chance = 0.1

	def __init__(self, gender=None, first_name=None, family_name=None,
			has_soul=None):
		self.gender = gender or pick_random(["female", "male"])
		self.gender = self.gender.lower().strip()
		self.first_name = first_name \
			or pick_random(self.first_names[self.gender])
		self.family_name = family_name \
			or pick_random(self.family_names["base"])
		self.family_name = self.gender_family_name(
			self.family_name, self.gender)
		if has_soul == None:
			self.has_soul = bool(random.random() < self.soul_chance)

	def __repr__(self):
		return "%s%s" % (
			self.full_name(),
			"" if self.has_soul else " (no soul)")

	def gender_family_name(self, family_name, gender):
		family_name_parsed = family_name.split(">")
		# No suffix marker or not modified for the gender
		if not len(family_name_parsed) > 1 or gender[0] not in family_name_parsed[-1]:
			return family_name_parsed[0]
		suffix_instructions = re.findall(r"([%s])([a-z]*)" % gender[0], family_name_parsed[-1])
		if not suffix_instructions:
			raise Exception("Something went wrong trying to parse the suffix \
				encoding \"%s\" for family name \"%s\"." \
				% (family_name_parsed[-1], family_name))
		return family_name_parsed[0] + suffix_instructions[0][1] + self.family_names[gender]

	def random_name(self, gender=None, first_name=None, family_name=None):
		genders = ["female", "male"]
		if not gender:
			gender = pick_random(genders)
		gender = gender.lower().strip()
		if not gender in genders:
			raise Exception("Gender \"%s\" is not valid. Expect one of: \"%s\"." % (gender, "\", \"".join(genders)))
		# Pick first name
		if not first_name:
			first_name = pick_random(first_names[gender])
		# Pick family name
		if not family_name:
			family_name = pick_random(family_names["base"])
		gendered_family_name = gender_family_name(family_name, gender)
		full_name = "%s %s, %s" % (first_name, gendered_family_name, gender)
		return full_name

	def full_name(self):
		return "%s %s, %s" % (self.first_name, self.family_name, self.gender)

if __name__ == "__main__":
	if len(sys.argv) > 1:
		for x in range(int(sys.argv[-1])):
			print StrahdNPC()
