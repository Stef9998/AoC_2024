import python.day14.utils as ut
from python.day14.utils import parse_lines, Robot
from python.file_handling import get_specific_file_as_lines
from python.map_handling import Coordinate


def main(lines: list[str]) -> int:
    """
    Calculates the product of quadrant counts after simulating robot movements.

    >>> lines = [
    ...     "p=0,4 v=3,-3",
    ...     "p=6,3 v=-1,-3",
    ...     "p=10,3 v=-1,2",
    ...     "p=2,0 v=2,-1",
    ...     "p=0,0 v=1,3",
    ...     "p=3,0 v=-2,-2",
    ...     "p=7,6 v=-1,-3",
    ...     "p=3,0 v=-1,-2",
    ...     "p=9,3 v=2,3",
    ...     "p=7,3 v=-1,2",
    ...     "p=2,4 v=2,-3",
    ...     "p=9,5 v=-3,-3",
    ... ]
    >>> ut.map_width = 11
    >>> ut.map_height = 7
    >>> ut.map_left_right_split = 5
    >>> ut.map_upper_lower_split = 3
    >>> main(lines)
    12
    """

    robots = parse_lines(lines)

    robot_coordinates = map(lambda robot: get_coordinates_after_n_iterations(robot, 100), robots)
    coordinates_quadrant = map(get_coordinate_quadrant, robot_coordinates)
    quadrant_counts = tuple(map(sum, zip(*coordinates_quadrant)))

    return quadrant_counts[0] * quadrant_counts[1] * quadrant_counts[2] * quadrant_counts[3]


def get_coordinate_quadrant(coordinate: Coordinate) -> tuple[int, int, int, int]:
    if coordinate.x < ut.map_left_right_split:
        if coordinate.y < ut.map_upper_lower_split:
            return 1, 0, 0, 0
        elif coordinate.y > ut.map_upper_lower_split:
            return 0, 0, 1, 0
    elif coordinate.x > ut.map_left_right_split:
        if coordinate.y < ut.map_upper_lower_split:
            return 0, 1, 0, 0
        elif coordinate.y > ut.map_upper_lower_split:
            return 0, 0, 0, 1
    return 0, 0, 0, 0


def get_coordinates_after_n_iterations(robot: Robot, n: int) -> Coordinate:
    """
    Calculates the coordinates of a robot after n iterations.

    :param robot: A tuple of two Coordinates representing the initial position and velocity.
    :param n: The number of iterations to simulate.
    :return: The final Coordinate after n iterations.
    """
    initial_position, velocity = robot.initial_position, robot.velocity
    return Coordinate(
        (initial_position.x + velocity.x * n) % ut.map_width,
        (initial_position.y + velocity.y * n) % ut.map_height
    )


if __name__ == '__main__':
    # result = main(get_file_as_lines())
    result = main(get_specific_file_as_lines('sample_input.txt'))
    print(f"Part one result:\n{result}")
    # assert result ==
