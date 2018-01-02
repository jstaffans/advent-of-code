from multiprocessing import Manager
from concurrent.futures import ProcessPoolExecutor, as_completed


def severity(input, delay=0):
    layers = [map(int, layer.split(": ")) for layer in input.splitlines()]
    layer_lookup = {x: y for x, y in layers}
    layer = 0
    timer = delay
    damage = 0

    while len(layer_lookup) > 0:
        scanner = layer_lookup.pop(layer, None)

        if scanner and timer % (2 * (scanner - 1)) == 0:
            # damage += layer * scanner
            damage += 1

        layer += 1
        timer += 1

    return damage


def search(data, initial, step, q):
    delay = initial
    while severity(data, delay) > 0 and q.empty():
        delay += step
    q.put(delay)


with open("data/day13.txt") as f, ProcessPoolExecutor(4) as executor:
    # data = "0: 3\n1: 2\n4: 4\n6: 4"
    data = f.read()
    manager = Manager()
    queue = manager.Queue()
    for initial in range(4):
        executor.submit(search, data, initial, 4, queue)
    print(queue.get())
