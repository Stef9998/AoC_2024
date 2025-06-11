from enum import Enum
from typing import Iterator

from file_handling import input_as_lines
from map_handling import get_map_dimensions, Coordinate, Direction

DOCTEST_MAP: list[str] = [
    "#####",
    "#...#",
    "#.S.#",
    "#...#",
    "#.#E#",
    "#####"
]


class Tile(Enum):
    EMPTY = '.'
    WALL = '#'
    START = 'S'
    END = 'E'

    def __str__(self) -> str:
        return self.value

    @staticmethod
    def get_tile(char: str) -> 'Tile':
        """
        Returns the Tile enum corresponding to the character.

        :param char: A character representing a tile type.
        :return: The Tile enum corresponding to the character.
        :raises ValueError: If the character is not a valid tile type.
        """
        if char == '.':
            return Tile.EMPTY
        elif char == '#':
            return Tile.WALL
        elif char == 'S':
            return Tile.START
        elif char == 'E':
            return Tile.END
        else:
            raise ValueError(f"Invalid tile character: {char}")


def get_map_information(lines: list[str]):
    height, width = get_map_dimensions(lines)
    start_coordinate, end_coordinate = information_functional(lines)

    return height, width, start_coordinate


def information_functional(lines):
    start_coordinate = next(
        (Coordinate(x, y) for y, row in enumerate(lines) for x, char in enumerate(row)
         if Tile.get_tile(char) == Tile.START),
        None
    )
    end_coordinate = next(
        (Coordinate(x, y) for y, row in enumerate(lines) for x, char in enumerate(row)
         if Tile.get_tile(char) == Tile.END),
        None
    )
    return start_coordinate, end_coordinate


def print_map(map_data: list[str], reindeer_coordinate: Coordinate, direction, visited) -> None:
    """
    Prints the map data to the console, highlighting the reindeer's position.

    Args:
        map_data (list[list[Tile]]): A 2D array representing the map, where each element is a Tile.
        reindeer_coordinate (Coordinate): The current coordinate of the reindeer.
        direction:
        visited:
    """
    for y, row in enumerate(map_data):
        print("".join(
            dir_to_char(direction) if Coordinate(x, y) == reindeer_coordinate else
            'o' if Coordinate(x, y) in visited else tile
            for x, tile in enumerate(row)
        ))
    print()


def dir_to_char(direction: Direction):
    if direction == Direction.UP:
        return '^'
    elif direction == Direction.DOWN:
        return 'v'
    elif direction == Direction.LEFT:
        return '<'
    elif direction == Direction.RIGHT:
        return '>'


def get_next_turn_direction(coordinate: Coordinate, direction: Direction, map_data,
                            visited: set[Coordinate]) -> Direction:
    """
    Determines the next turn direction from the given coordinate and direction.

    This function checks if the left or right turn is possible from the current position
    and returns the corresponding direction. If neither turn is possible, it raises an error.

    Args:
        coordinate (Coordinate): The current position on the map.
        direction (Direction): The current movement direction.
        map_data (list[str]): The map layout represented as a list of strings.
        visited (set[Coordinate]): A set of coordinates that have already been visited.

    Returns:
        Direction: The next valid turn direction.

    Raises:
        ValueError: If neither left nor right turn is possible.

    Examples:
        >>> from python.day16.utils import DOCTEST_MAP as map_data

        >>> get_next_turn_direction(Coordinate(1,4), Direction.DOWN, map_data, set())
        Traceback (most recent call last):
        ...
        ValueError: Both turning left and right are not possible, as can not step on these tiles
        >>> get_next_turn_direction(Coordinate(1,3), Direction.UP, map_data, set())
        <Direction.RIGHT: Coordinate(1, 0)>
        >>> get_next_turn_direction(Coordinate(1,1), Direction.RIGHT, map_data, set())
        <Direction.DOWN: Coordinate(0, 1)>
        >>> get_next_turn_direction(Coordinate(1,1), Direction.RIGHT, map_data, {Coordinate(2,1)})
        <Direction.DOWN: Coordinate(0, 1)>
    """
    if can_step_on_tile(coordinate + direction.turn_left().value, map_data, visited):
        return direction.turn_left()
    if can_step_on_tile(coordinate + direction.turn_right().value, map_data, visited):
        return direction.turn_right()
    else:
        raise ValueError(f"Both turning left and right are not possible, as can not step on these tiles")


def is_straight_no_branch(coordinate: Coordinate, direction: Direction, map_data, visited: set[Coordinate]) -> bool:
    if branch_factor(coordinate, direction, map_data, visited) != 1:
        return False
    if not can_step_on_tile(coordinate + direction.value, map_data, visited):
        return False

    return True


def is_branch(coordinate: Coordinate, direction: Direction, map_data, visited: set[Coordinate]) -> bool:
    """
    >>> from python.day16.utils import DOCTEST_MAP as map_data

    >>> is_branch(Coordinate(1,4), Direction.DOWN, map_data, set())
    False
    >>> is_branch(Coordinate(1,4), Direction.UP, map_data, set())
    False
    >>> is_branch(Coordinate(1,1), Direction.RIGHT, map_data, set())
    True
    >>> is_branch(Coordinate(2,1), Direction.RIGHT, map_data, {Coordinate(3,1)})
    False
    >>> is_branch(Coordinate(1,3), Direction.DOWN, map_data, set())
    True
    >>> is_branch(Coordinate(1,3), Direction.DOWN, map_data, {Coordinate(1,4)})
    False
    >>> is_branch(Coordinate(1,3), Direction.DOWN, map_data, {Coordinate(2,3)})
    False
    """
    if branch_factor(coordinate, direction, map_data, visited) > 1:
        return True
    else:
        return False


def branch_factor(coordinate: Coordinate, direction: Direction, map_data, visited: set[Coordinate]) -> int:
    """
    Calculates the branch factor for a given coordinate and direction.

    The branch factor is the number of valid directions that can be taken from the given coordinate
    and direction, considering the map layout and visited coordinates.

    :param coordinate: The `Coordinate` object representing the current position.
    :param direction: The `Direction` object representing the movement direction.
    :param map_data: A list of strings representing the map layout.
    :param visited: A set of `Coordinate` objects that have already been visited.
    :return: An integer representing the branch factor.

    >>> from python.day16.utils import DOCTEST_MAP as map_data

    >>> branch_factor(Coordinate(1,4), Direction.DOWN, map_data, set())
    0
    >>> branch_factor(Coordinate(1,4), Direction.UP, map_data, set())
    1
    >>> branch_factor(Coordinate(1,1), Direction.RIGHT, map_data, set())
    2
    >>> branch_factor(Coordinate(2,1), Direction.RIGHT, map_data, {Coordinate(3,1)})
    0
    >>> branch_factor(Coordinate(1,3), Direction.DOWN, map_data, set())
    2
    >>> branch_factor(Coordinate(1,3), Direction.DOWN, map_data, {Coordinate(1,4)})
    1
    >>> branch_factor(Coordinate(1,3), Direction.DOWN, map_data, {Coordinate(2,3)})
    1
    >>> branch_factor(Coordinate(1,3), Direction.RIGHT, map_data, set())
    3
    >>> branch_factor(Coordinate(3,3), Direction.LEFT, map_data, set())
    3
    """
    can_step_on_surrounding_tile = lambda _direction: can_step_on_tile(coordinate + _direction.value, map_data, visited)
    return sum(map(can_step_on_surrounding_tile, [direction, direction.turn_left(), direction.turn_right()]))
    # return (can_step_on_tile(coordinate + direction.value, map_data, visited) +
    #         can_step_on_tile(coordinate + direction.turn_right().value, map_data, visited) +
    #         can_step_on_tile(coordinate + direction.turn_left().value, map_data, visited))


def is_dead_end(coordinate: Coordinate, direction: Direction, map_data, visited: set[Coordinate]) -> bool:
    """
    Determines if the given coordinate and direction lead to a dead end.

    :param coordinate: The current `Coordinate` object to evaluate.
    :param direction: The `Direction` object representing the movement direction.
    :param map_data: A list of strings representing the map layout.
    :param visited: A set of `Coordinate` objects representing previously visited branch points.
    :return: `True` if the coordinate and direction lead to a dead end, `False` otherwise.

    >>> from python.day16.utils import DOCTEST_MAP as map_data

    >>> is_dead_end(Coordinate(1,4), Direction.DOWN, map_data, set())
    True
    >>> is_dead_end(Coordinate(1,4), Direction.UP, map_data, set())
    False
    >>> is_dead_end(Coordinate(1,1), Direction.RIGHT, map_data, set())
    False
    >>> is_dead_end(Coordinate(2,1), Direction.RIGHT, map_data, {Coordinate(3,1)})
    True
    """
    if branch_factor(coordinate, direction, map_data, visited) == 0:
        return True
    else:
        return False


def is_wall(map_data, coordinate: Coordinate) -> bool:
    return get_tile(coordinate, map_data) == Tile.WALL


def get_start_directions(map_data, coordinate: Coordinate) -> Iterator[Direction]:
    """
    Determines the possible directions to move from a given starting coordinate.

    :param map_data: A list of strings representing the map layout.
    :param coordinate: The current `Coordinate` to evaluate for possible movement directions.
    :return: An iterator of `Direction` objects representing valid movement directions.

    >>> from python.day16.utils import DOCTEST_MAP as map_data
    >>> list(get_start_directions(map_data, Coordinate(2,2)))
    [<Direction.UP: Coordinate(0, -1)>, <Direction.RIGHT: Coordinate(1, 0)>, <Direction.DOWN: Coordinate(0, 1)>, <Direction.LEFT: Coordinate(-1, 0)>]
    >>> list(get_start_directions(map_data, Coordinate(1,1)))
    [<Direction.RIGHT: Coordinate(1, 0)>, <Direction.DOWN: Coordinate(0, 1)>]
    """
    # return (direction for direction in Direction if can_step_on_tile(coordinate+direction.value, map_data, set()))
    can_step_on_surrounding_tile = lambda direction: can_step_on_tile(coordinate + direction.value, map_data, set())
    return filter(can_step_on_surrounding_tile, Direction)


def get_next_possible_directions(coordinate: Coordinate, direction: Direction, map_data, visited: set[Coordinate]) ->\
        Iterator[Direction]:
    can_step_on_surrounding_tile = lambda _direction: can_step_on_tile(coordinate + _direction.value, map_data, visited)
    return filter(can_step_on_surrounding_tile, [direction, direction.turn_left(), direction.turn_right()])


def can_step_on_tile(coordinate: Coordinate, map_data, visited: set[Coordinate]) -> bool:
    """
    Determines if a tile can be stepped on based on its type and visited status.

    :param coordinate: The `Coordinate` object representing the position to check.
    :param map_data: A list of strings representing the map layout.
    :param visited: A set of `Coordinate` objects that have already been visited.
    :return: `True` if the tile can be stepped on, `False` otherwise.

    >>> from python.day16.utils import DOCTEST_MAP as map_data
    >>> can_step_on_tile(Coordinate(0,0), map_data, set())
    False
    >>> can_step_on_tile(Coordinate(1,1), map_data, set())
    True
    >>> can_step_on_tile(Coordinate(2,2), map_data, set())
    False
    """
    tile = get_tile(coordinate, map_data)
    if is_wall(map_data, coordinate):
        return False
    if coordinate in visited:
        return False
    if tile == Tile.START:
        return False

    if tile == Tile.EMPTY or tile == Tile.END:
        return True
    else:
        raise ValueError(f"Configuration of map is broken. Wanted to test Coordinate: {coordinate}."
                         f" Can't determine if can step on this tile.")


def get_tile(coordinate: Coordinate, map_data) -> Tile:
    return Tile.get_tile(map_data[coordinate.y][coordinate.x])


if __name__ == '__main__':
    import time

    start_time = time.time()
    result = get_map_information(input_as_lines())
    print(f"Part one result:\n{result}")
    print(f"Time taken: {time.time() - start_time:.3f} seconds")
