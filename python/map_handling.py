# type Coordinate = tuple[int, int]

# TODO: Test if Coordinate class works the same as tuple[int, int]
class Coordinate:
    __slots__ = ('x', 'y')  # Restrict attributes to x and y for immutability

    def __init__(self, x: int, y: int):
        self.x = x
        self.y = y

    def __setattr__(self, key, value):
        if hasattr(self, key):
            raise AttributeError(f"{self.__class__.__name__} is immutable")
        super().__setattr__(key, value)

    def __getitem__(self, index: int) -> int:
        if index == 0:
            return self.x
        elif index == 1:
            return self.y
        else:
            raise IndexError("Coordinate index out of range")

    def __iter__(self):
        return iter((self.x, self.y))

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Coordinate):
            return self.x == other.x and self.y == other.y
        return False

    def __add__(self, other: 'Coordinate') -> 'Coordinate':
        return Coordinate(self.x + other.x, self.y + other.y)

    def __sub__(self, other: 'Coordinate') -> 'Coordinate':
        return Coordinate(self.x - other.x, self.y - other.y)

    def __mul__(self, scalar: int) -> 'Coordinate':
        return Coordinate(self.x * scalar, self.y * scalar)

    def __rmul__(self, scalar: int) -> 'Coordinate':
        return self * scalar

    def __hash__(self) -> int:
        return hash((self.x, self.y))

    def __repr__(self) -> str:
        return f"Coordinate({self.x}, {self.y})"