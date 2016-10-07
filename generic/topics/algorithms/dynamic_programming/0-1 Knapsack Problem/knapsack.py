import re

DEBUG = True

class Item(object):
    def __init__(self, name, weight, value):
        self._name = name
        self._weight = weight
        self._value = value

    @property
    def name(self):
        return self._name

    @property
    def weight(self):
        return self._weight

    @property
    def value(self):
        return self._value


class Solver(object):
    def __init__(self, max_weight):
        self._items = set()
        self._max_weight = max_weight

    def add_item(self, item):
        self._items.add(item)

    def solve(self):
        items = sorted(self._items, key=lambda item: item.weight)
        solution_table = [[0] * (self._max_weight+1)] + [
            [0] * (self._max_weight+1)
            for _ in items
        ]

        for w in range(1, self._max_weight + 1):
            for item_idx, item in enumerate(items):
                if item.weight > w:
                    solution_table[item_idx+1][w] = solution_table[item_idx][w]
                else:
                    solution_table[item_idx+1][w] = max(
                        solution_table[item_idx][w-item.weight] + item.value,
                        solution_table[item_idx][w]
                    )

        return solution_table[-1][-1]

if __name__ == '__main__':
    if DEBUG:
        items = [
            Item("Apple", 3, 2),
            Item("Pear", 4, 4),
            Item("Cherry", 1, 5),
            Item("Pineapple", 5, 5)
        ]
        max_weight = 10
    else:
        item_regex = re.compile(r'''(?P<name>\w+),(?P<weight>\d+),(?P<value>\d+)''')
        items = []
        while True:
            item = input(
                '''Add an item (in the form of "name,weight,value"): '''
            )
            if not item:
                break
            g = item_regex.match(item)
            if g is None:
                print("Bad format!")
                continue
            name, weight, value = g.group("name"), int(g.group("weight")), int(g.group("value"))
            items.append(Item(name, weight, value))
        while True:
            max_weight = input(
                '''Maximum weight: '''
            )
            try:
                max_weight = int(max_weight)
                break
            except ValueError:
                print("Bad format!")
                continue
    solver = Solver(max_weight)
    for item in items:
        solver.add_item(item)
    print("Solution: %d" % solver.solve())
