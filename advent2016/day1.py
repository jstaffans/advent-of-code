from common import input, rotate_left, rotate_right

N, S, W, E = 1j, -1j, -1, 1

def moves(s):
    "Parse moves from input string."
    dirs = [N, W, S, E]  # start facing north
    parts = s.split(", ")
    position = (0+0j)

    for p in parts:
        direction = p[0]
        distance = int(p[1:])
        dirs = rotate_left(dirs) if direction == 'L' else rotate_right(dirs)
        position += distance * dirs[0]

    return position

def distance(position):
    return abs(position.imag) + abs(position.real)


# part two: first place visited twice

def first_visited_twice(s):
    dirs = [N, W, S, E]  # start facing north
    parts = s.split(", ")
    total = []
    position = (0+0j)
    visited = {}

    for p in parts:
        direction = p[0]
        distance = int(p[1:])
        dirs = rotate_left(dirs) if direction == 'L' else rotate_right(dirs)
        for i in range(distance):
            visited[position] = True
            position += dirs[0]
            if visited.get(position, False):
                return position

    return position


distance(moves(input(2016, 1).read()))
distance(first_visited_twice(input(2016, 1).read()))

