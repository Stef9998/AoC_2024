from day6.utils import init_maze_findings
from file_handling import input_as_lines
from map_handling import Coordinate, Direction


def main(lines: list[str]) -> int:
    maze_information = init_maze_findings(lines)

    maze_width = maze_information["maze_width"]
    maze_height = maze_information["maze_height"]

    obstacle_positions = maze_information["obstacle_positions"]
    guard_position = maze_information["guard_start"]
    guard_direction = maze_information["guard_direction"]

    outside_bounds = lambda position: (
            position.x < 0 or position.x >= maze_width
            or position.y < 0 or position.y >= maze_height
    )
    get_next_guard_state = lambda position, direction: (
        get_next_guard_state_with_obstacles(position, direction, obstacle_positions)
    )

    visited: set[Coordinate] = set()

    while True:
        visited.add(guard_position)
        guard_position, guard_direction = get_next_guard_state(guard_position, guard_direction)
        if outside_bounds(guard_position):
            break

    return len(visited)


def get_next_guard_state_with_obstacles(guard_position: Coordinate, guard_direction, obstacle_positions):
    next_position = calc_next_position(guard_direction, guard_position)
    if next_position not in obstacle_positions:
        return next_position, guard_direction
    return guard_position, guard_direction.turn_right()


def calc_next_position(guard_direction: Direction, guard_position: Coordinate) -> Coordinate:
    return guard_position + guard_direction.value


if __name__ == '__main__':
    part_one_result = main(input_as_lines())
    print(f"Part one result:\n{part_one_result}")
    assert part_one_result == 4711, f'Expected 4711, but got {part_one_result}'
