class Node:
    def __init__(self, *, up=None, down=None, right=None, left=None):
        (
            self.up, self.down,
            self.right, self.left
        ) = (
            up, down,
            right, left
        )
