import urllib.request

# common helpers for the advent of code puzzles

def input(year, day):
    "Today's input file."
    filename = 'data/input{}.txt'.format(day)
    try:
        return open(filename)
    except FileNotFoundError:
        return urllib.request.urlopen('http://bitrite.fi/advent/{}/day/{}/input'.format(year, day))

def rotate_left(l):
    "Rotate list one step left."
    return l[1:] + l[:1]

def rotate_right(l):
    "Rotate list one step right."
    return l[-1:] + l[:-1]
