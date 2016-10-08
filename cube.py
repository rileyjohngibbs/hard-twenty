import copy

from pprint import pprint

'''
	counter(W):
		WRB -> WBO

	clock(B):
		WRB -> OWB
'''

cubies = [
	"WRB", "WBO", "WOG", "WGR",
	"YRG", "YGO", "YOB", "YBR",
	"WB", "WO", "WG", "WR",
	"RB", "BO", "OG", "GR",
	"BY", "OY", "GY", "RY"
]

starting_position = {x:x for x in cubies} # key is requirement for solve; value is current cubie
cube = copy.deepcopy(starting_position)

face_turns = { # start: (destination, twists)
	"red": {
		"WGR": ("YRG", 2),
		"WRB": ("WGR", 1),
		"YBR": ("WRB", 2),
		"YRG": ("YBR", 1)
	},
	"blue": {
		"WBO": ("WRB", 1),
		"YOB": ("WBO", 2),
		"YBR": ("YOB", 1),
		"WRB": ("YBR", 2)
	}
}

def same_piece(pieces):
	for piece in pieces:
		for other_piece in filter(lambda x: x != piece, pieces):
			if any(map(lambda x: x not in other_piece, piece)):
				return False
	return True

def cube_state(permutation):
	position_solved = not any(map(lambda x: not same_piece(x), permutation.iteritems()))
	full_solved = not any(map(lambda x: not x[0]==x[1], permutation.iteritems()))
	# Make sure that all of the edges are in edge spaces and all of the corners are in corner spaces
	valid_state = not any(map(lambda x: not len(x[0])==len(x[1]), permutation.iteritems()))
	return {"full_solved": full_solved, "position_solved": position_solved, "valid_state": valid_state}

def twist_cubie(cubie, twists=1):
	# Permutation (1 2 3) or (1 2)
	for x in range(twists):
		cubie = cubie[-1] + cubie[:-1]
	return cubie

def turn_face(cube, face):
	new_pos = {}
	for cubie in cube:
		if cubie in face_turns[face]:
			new_pos[face_turns[face][cubie][0]] = twist_cubie(cube[cubie], face_turns[face][cubie][1])
	cube.update(new_pos)
