from enum import Enum
from typing import Callable

from dataclasses import dataclass


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

    def __str__(self) -> str:
        return f"({self.x}, {self.y})"

    def __repr__(self) -> str:
        return f"Coordinate({self.x}, {self.y})"


@dataclass
class Map:
    width: int
    height: int
    out_of_bounds_calculator: Callable[[Coordinate], bool]

    def __init__(self, width: int, height: int, **optional_information):
        self.width = width
        self.height = height
        for key, value in optional_information.items():
            setattr(self, key, value)
        self.out_of_bounds_calculator = out_of_bounds_calculator(self.width, self.height)

    def __repr__(self) -> str:
        attributes = [f"{key}={value!r}" for key, value in self.__dict__.items()]
        return "{}({})".format(type(self).__name__, ", ".join(attributes))


_map_instance: 'Map' = None  # Module-level variable for singleton instance


def get_map_instance(*args, **kwargs) -> 'Map':
    """
    Returns the singleton instance of the Map class. If it doesn't exist, it creates one.
    """
    global _map_instance
    if _map_instance is None:
        _map_instance = Map(*args, **kwargs)
    return _map_instance

def initialise_map(width: int, height: int, **optional_information) -> 'Map':
    """
    Initializes the map instance with the given width, height, and optional information.
    If the map instance already exists, it raises an error.

    Args:
        width (int): The width of the map.
        height (int): The height of the map.
        **optional_information: Additional optional information about the map.

    Returns:
        Map: The initialized map instance.
    """
    global _map_instance
    if _map_instance is not None:
        raise ValueError("Map instance already initialized.")
    _map_instance = Map(width, height, **optional_information)
    return _map_instance


def out_of_bounds(position: Coordinate, maze_width: int, maze_height: int) -> bool:
    return position.x < 0 or position.x >= maze_width or position.y < 0 or position.y >= maze_height

def out_of_bounds_calculator(maze_width: int, maze_height: int) -> Callable[[Coordinate], bool]:
    def out_of_bounds_closure(position: Coordinate) -> bool:
        return out_of_bounds(position, maze_width, maze_height)
    return out_of_bounds_closure


class Direction(Enum):
    UP = Coordinate(0, -1)
    RIGHT = Coordinate(1, 0)
    DOWN = Coordinate(0, 1)
    LEFT = Coordinate(-1, 0)

    @property
    def x(self) -> int:
        return self.value[0]

    @property
    def y(self) -> int:
        return self.value[1]

    def turn_right(self) -> 'Direction':
        directions = list(Direction)
        current_index = directions.index(self)
        return directions[(current_index + 1) % len(directions)]

    def turn_left(self) -> 'Direction':
        directions = list(Direction)
        current_index = directions.index(self)
        return directions[(current_index - 1) % len(directions)]

    def opposite_direction(self) -> 'Direction':
        directions = list(Direction)
        current_index = directions.index(self)
        return directions[(current_index + 2) % len(directions)]

    def __str__(self) -> str:
        return f"{self.name.capitalize()} {self.value}"


def get_surrounding_coordinates(coordinate: Coordinate) -> tuple[Coordinate, Coordinate, Coordinate, Coordinate]:
    return (coordinate + Direction.RIGHT.value, coordinate + Direction.DOWN.value,
            coordinate + Direction.LEFT.value, coordinate + Direction.UP.value)


def get_map_dimensions(map_data: list[str]) -> tuple[int, int]:
    """
    Get the dimensions of the map.

    Args:
        map_data (list[str]): The map represented as a list of strings. Each character in the string represents a cell in the map.

    Returns:
        tuple[int, int]: The height and width of the map.
    """
    return len(map_data), len(map_data[0])
