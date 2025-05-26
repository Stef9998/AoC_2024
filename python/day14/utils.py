from collections.abc import Iterator

from python.map_handling import Coordinate

from dataclasses import dataclass


@dataclass
class Robot:
    initial_position: Coordinate
    velocity: Coordinate


map_width = 101
map_height = 103

map_left_right_split = 50
map_upper_lower_split = 51


def parse_lines(lines: list[str]) -> Iterator[Robot]:
    """
    Parses lines using the provided parser function.
    The parser function is for parsing a single line.

    :param lines: list of lines to parse. Lines are strings.
    :return: An iterator of Robots.
    """
    return map(parser, lines)


def parser(line: str):
    import re
    match = re.match(r"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)", line)
    if match:
        x1, y1, x2, y2 = map(int, match.groups())
        return Robot(Coordinate(x1, y1), Coordinate(x2, y2))
    return None
