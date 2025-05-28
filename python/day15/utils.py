from collections.abc import Iterator
from enum import Enum

from python.file_handling import get_specific_file_as_lines
from python.map_handling import Direction, Coordinate


class Tile(Enum):
    EMPTY = '.'
    WALL = '#'
    ROBOT = '@'
    BOX = 'O'

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
        elif char == '@':
            return Tile.ROBOT
        elif char == 'O':
            return Tile.BOX
        else:
            raise ValueError(f"Invalid tile character: {char}")



def main(lines: list[str]) -> tuple[dict[Tile, set[Coordinate]], Iterator[Direction]]:
    map_string, movement_string = split_input_data(lines)

    movements = map(movement_char_to_direction, movement_string)

    parsed_lines = map(parse_map_line_enumerate, map_string)
    tiles = list((Coordinate(x, y), tile) for y, parsed_line in enumerate(parsed_lines) for x, tile in parsed_line)
    tile_sets = {tile: {coordinate for coordinate, t in tiles if t == tile} for tile in Tile}

    # TODO: instead of using sets maybe just use a 2d array (list)
    #  then I can easier test for the coordinates line of sight of the robot

    return tile_sets, movements

def main_2d_array(lines: list[str]) -> tuple[list[list[Tile]], Coordinate, Iterator[Direction]]:
    """
    Parses the input data into a 2D array of Tile enums and an iterable of movement directions.

    :param lines: A list of strings representing the input data.
    :return: A tuple containing a 2D array of Tile enums and an iterable of movement directions.
    """
    map_string, movement_string = split_input_data(lines)

    movements = map(movement_char_to_direction, movement_string)

    robot_coordinate = get_robot_coordinate(map_string)
    map_data = [parse_map_replace_robot(line) for line in map_string]

    return map_data, robot_coordinate, movements


def split_input_data(lines: list[str]) -> tuple[list[str], str]:
    """
    Splits the input data into map data and movement data.

    :param lines: A list of strings representing the input data. The map data is separated from the movement data by an empty line.
    :return: A tuple containing the map data as a list of strings and the movement data as a single string.
    """
    empty_line_index = lines.index("")
    map_data = lines[:empty_line_index]
    movement_data = "".join(lines[empty_line_index + 1:])
    return map_data, movement_data


def parse_map_line_enumerate(line: str) -> Iterator[tuple[int, Tile]]:
    return enumerate(parse_map_line(line))


def parse_map_line(line):
    return map(Tile.get_tile, line)


def parse_map_replace_robot(line: str) -> list[Tile]:
    parsed = parse_map_line(line)
    return [tile if tile != Tile.ROBOT else Tile.EMPTY for tile in parsed]


def get_robot_coordinate(map_data: list[str]) -> Coordinate:
    """
    Finds the robot's coordinate in the map data.

    :param map_data: A list of strings representing the map.
    :return: The Coordinate of the robot if found, otherwise None.
    """
    for y, line in enumerate(map_data):
        for x, char in enumerate(line):
            if char == Tile.ROBOT.value:
                return Coordinate(x, y)
    assert False, "Robot coordinate not found in map data"


def movement_char_to_direction(char: str) -> Direction:
    """
    Converts a character representing a movement direction to a Direction enum.

    :param char: A character representing the movement direction ('<', '>', '^', 'v').
    :return: A Direction enum corresponding to the character.
    :raises ValueError: If the character is not a valid movement direction.
    """
    if char == '^':
        return Direction.UP
    elif char == 'v':
        return Direction.DOWN
    elif char == '<':
        return Direction.LEFT
    elif char == '>':
        return Direction.RIGHT
    else:
        raise ValueError(f"Invalid movement character: {char}")

if __name__ == '__main__':
    # result = main(get_file_as_lines())
    result = main(get_specific_file_as_lines('sample_input.txt'))
