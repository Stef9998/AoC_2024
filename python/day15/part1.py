from typing import Optional

from python.file_handling import input_as_lines, input_as_lines
from python.map_handling import initialise_map, get_map_dimensions, Coordinate, Direction
from python.day15.utils import main as get_input_data
from python.day15.utils import main_2d_array as get_input_2d_array
from python.day15.utils import Tile


# TODO: Funny Idea: Make use of fact, that some boxes will never be movable again. (implementation 2)
#  This will happen, if a box is surrounded by walls on two sides that are not across from each other.
#  Also if a box is surrounded by two (not across from each other) boxes and these boxes are unmovable, it will also be unmovable.
#   So one solution would be to just make every box -> wall if unmovable.
#    But this would make editing the map (data) without it being correct, as a box is not a wall.
#   Other solution would be to mark the boxes (or tiles in general, as walls are always unmovable) as unmovable.

def main(lines: list[str]) -> int:
    map_data, robot_coordinate, movements = get_input_2d_array(lines)

    for movement in movements:

        next_coordinate = robot_coordinate + movement.value
        next_empty = next_empty_tile(map_data, robot_coordinate, movement)
        if next_empty is not None:
            map_data[next_empty.y][next_empty.x] = map_data[next_coordinate.y][next_coordinate.x]
            map_data[next_coordinate.y][next_coordinate.x] = Tile.EMPTY
            robot_coordinate = next_coordinate

    summed_gps_value = sum(
        100 * y + x for y, row in enumerate(map_data) for x, tile in enumerate(row) if tile == Tile.BOX
    )

    return summed_gps_value


def next_empty_tile(map_data: list[list[Tile]], robot_coordinate: Coordinate, direction: Direction) -> Optional[Coordinate]:
    """
    Finds the next empty tile in the given direction starting from the robot's current coordinate.

    Args:
        map_data (list[list[Tile]]): A 2D array representing the map, where each element is a Tile.
        robot_coordinate (Coordinate): The current coordinate of the robot.
        direction (Coordinate): The direction to move in, represented as a Coordinate.

    Returns:
        Optional[Coordinate]: The coordinate of the next empty tile if found, or None if a wall is encountered.
    """
    while True:
        next_coordinate = robot_coordinate + direction.value
        if map_data[next_coordinate.y][next_coordinate.x] == Tile.EMPTY:
            return next_coordinate
        if map_data[next_coordinate.y][next_coordinate.x] == Tile.WALL:
            return None
        robot_coordinate = next_coordinate


def print_map(map_data: list[list[Tile]], robot_coordinate: Coordinate) -> None:
    """
    Prints the map data to the console, highlighting the robot's position.

    Args:
        map_data (list[list[Tile]]): A 2D array representing the map, where each element is a Tile.
        robot_coordinate (Coordinate): The current coordinate of the robot.
    """
    for y, row in enumerate(map_data):
        print("".join(
            Tile.ROBOT.value if Coordinate(x, y) == robot_coordinate else tile.value
            for x, tile in enumerate(row)
        ))
    print()


if __name__ == '__main__':
    # result = main(input_as_lines())
    result = main(input_as_lines('sample.txt'))
    print(f"Part one result:\n{result}")
    # assert result ==
