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
        self.record = []
        self.pieds = []
        self.beatme = 1e10
        for i in xrange(self.height):
            self.pieds.append([1e10]*self.width)

    def get_pos(self, pos):
        if pos[0] < 0 or pos[0] >= self.height:
            return None
        if pos[1] < 0 or pos[1] >= self.width:
            return None
        return self.matrix[pos[0]][pos[1]]

    def travel(self):
        pos = (0, 0)
        return self._travel_direction('right', [], 0, (0, -1))

    def _travel_direction(self, direction, record, weight, pos):
        record = copy.copy(record)
        pos_diff = self._pos_map[direction]
        pos = (pos[0] + pos_diff[0], pos[1] + pos_diff[1])
        step_weight = self.get_pos(pos)
        if step_weight is None:
            return None
        record.append(step_weight)
        weight += step_weight
        if pos is None:
            return None
        if self.pieds[pos[0]][pos[1]] < weight:
            return None
        self.pieds[pos[0]][pos[1]] = weight
        if pos == (self.height - 1, self.width - 1):
            self.beatme = None
        ret = None
        retr = self._travel_direction('right', record, weight, pos)
        print("RETR:" + str(retr))
        if retr is not None:
            ret = retr
        retd = self._travel_direction('down', record, weight, pos)
        print("RETD:" + str(retd))
        if retd is not None:
            ret = retd
        retl = self._travel_direction('left', record, weight, pos)
        print("RETL:" + str(retl))
        if retl is not None:
            ret = retl
        retu = self._travel_direction('up', record, weight, pos)
        print("RETU:" + str(retu))
        if retu is not None:
            ret = retu
        return ret


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
print(travel.travel())
