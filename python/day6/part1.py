from day6.utils import init_maze_findings
from file_handling import input_as_lines
from map_handling import Coordinate, Direction


def main(lines: list[str]) -> int:
    maze_information = init_maze_findings(lines)

    maze_width = maze_information["maze_width"]
    maze_height = maze_information["maze_height"]

    outside_bounds = lambda position: (
            position.x < 0 or position.x >= maze_width
            or position.y < 0 or position.y >= maze_height
    )

    return len(get_visited(maze_information, outside_bounds))


def get_visited(maze_information, outside_bounds_calculator):
    obstacle_positions = maze_information["obstacle_positions"]
    guard_position = maze_information["guard_start"]
    guard_direction = maze_information["guard_direction"]
    get_next_guard_state = lambda position, direction: (
        get_next_guard_state_with_obstacles(position, direction, obstacle_positions)
    )
    visited: set[Coordinate] = set()
    while True:
        visited.add(guard_position)
        guard_position, guard_direction = get_next_guard_state(guard_position, guard_direction)
        if outside_bounds_calculator(guard_position):
            break
    return visited


def get_next_guard_state_with_obstacles(guard_position: Coordinate, guard_direction, obstacle_positions):
    next_position = calc_next_position(guard_direction, guard_position)
    if next_position not in obstacle_positions:
        return next_position, guard_direction
    return guard_position, guard_direction.turn_right()


def calc_next_position(guard_direction: Direction, guard_position: Coordinate) -> Coordinate:
    return guard_position + guard_direction.value


if __name__ == '__main__':
    import time

    result = main(input_as_lines('sample.txt'))
    print(f"Sample one result:\n{result}")
    assert result == 41

    start_time = time.time()
    result = main(input_as_lines())
    print(f"Part one result:\n{result}")
    assert result == 4711
    print(f"Time taken: {time.time() - start_time:.3f} seconds")
