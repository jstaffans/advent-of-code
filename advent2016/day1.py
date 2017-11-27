from common import input, rotate_left, rotate_right

N, S, W, E = 1j, -1j, -1, 1

def moves(s):
    "Parse moves from input string."
    dirs = [N, W, S, E]  # start facing north
    parts = s.split(", ")
    result = []

    for p in parts:
        direction = p[0]
        distance = int(p[1:])
        dirs = rotate_left(dirs) if direction == 'L' else rotate_right(dirs)
        result.append(distance * dirs[0])

    return result

def distance(ms):
    all_moves = sum(ms)
    return abs(all_moves.imag) + abs(all_moves.real)

distance(moves(input(2016, 1).read()))

