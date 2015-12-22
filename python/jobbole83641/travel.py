#coding: utf-8

import copy

class Travel(object):
    _pos_map = {
        'up': (-1, 0),
        'down': (1, 0),
        'left': (0, -1),
        'right': (0, 1)
    }
    def __init__(self, matrix):
        self.matrix = copy.deepcopy(matrix)
        self.height = len(self.matrix)
        self.width = len(self.matrix[0])
        self.pieds = []
        self.beatme = None
        self.beatmesteps = None
        for _ in xrange(self.height):
            self.pieds.append([1e10]*self.width)

    def get_pos(self, pos):
        if pos[0] < 0 or pos[0] >= self.height:
            return None
        if pos[1] < 0 or pos[1] >= self.width:
            return None
        return self.matrix[pos[0]][pos[1]]

    def travel(self):
        pos = (0, 0)
        self._travel_direction('right', [], [], 0, (0, -1))

    def _travel_direction(self, direction, steps, record, weight, pos):
        record = copy.copy(record)
        steps = copy.copy(steps)
        pos_diff = self._pos_map[direction]
        pos = (pos[0] + pos_diff[0], pos[1] + pos_diff[1])
        steps.append(pos)
        step_weight = self.get_pos(pos)
        if step_weight is None:
            return None
        record.append(step_weight)
        weight += step_weight
        if pos is None:
            return None
        if self.pieds[pos[0]][pos[1]] < weight:
            return None
        if pos == (self.height - 1, self.width - 1):
            if self.pieds[pos[0]][pos[1]] > weight:
                self.beatme = record
                self.beatmesteps = steps
        self.pieds[pos[0]][pos[1]] = weight
        self._travel_direction('right', steps, record, weight, pos)
        self._travel_direction('down', steps, record, weight, pos)
        self._travel_direction('left', steps, record, weight, pos)
        self._travel_direction('up', steps, record, weight, pos)

    def pretty_print(self):
        matrix = copy.deepcopy(self.matrix)
        for cell in self.beatmesteps:
            matrix[cell[0]][cell[1]] = "."

        for row in matrix:
            print("[%s]" % " ".join(map(str, row)))


matrix = [
    [1, 1, 1, 1, 1, 1, 1],
    [9, 9, 9, 9, 9, 1, 9],
    [9, 1, 1, 1, 9, 1, 9],
    [9, 1, 9, 1, 1, 1, 9],
    [9, 1, 9, 9, 9, 9, 9],
    [9, 1, 1, 1, 1, 1, 1],
    [9, 9, 9, 9, 9, 9, 1]
]

travel = Travel(matrix)
travel.travel()
# print(travel.beatme)
travel.pretty_print()
