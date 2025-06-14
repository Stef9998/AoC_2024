from typing import Optional

import day6.part1 as p1
from day6.utils import init_maze_findings
from file_handling import input_as_lines
from map_handling import Coordinate, Direction


def main(lines: list[str]) -> int:
    maze_information = init_maze_findings(lines)

    maze_width = maze_information["maze_width"]
    maze_height = maze_information["maze_height"]

    obstacle_positions = maze_information["obstacle_positions"]
    guard_start_position = maze_information["guard_start"]
    guard_start_direction = maze_information["guard_direction"]

    outside_bounds = lambda position: (
            position.x < 0 or position.x >= maze_width
            or position.y < 0 or position.y >= maze_height
    )

    og_visited = p1.get_visited(maze_information, outside_bounds)

    loop_obstacles: set[Coordinate] = set()
    loop_obstacle_count = 0
    for i, new_obstacle_position in enumerate(og_visited):
        print(i, new_obstacle_position)
        new_obstacle_positions = obstacle_positions.union({new_obstacle_position})
        loops = does_loop(guard_start_position, guard_start_direction, new_obstacle_positions, outside_bounds)
        print(loops)
        if loops:
            loop_obstacle_count += 1
            loop_obstacles.update({new_obstacle_position})
    return loop_obstacle_count


def does_loop(guard_start_position, guard_start_direction, obstacle_positions, outside_bounds_calculator):
    guard_position = guard_start_position
    guard_direction = guard_start_direction
    get_next_guard_state = lambda position, direction: (
        get_next_guard_state_with_obstacles(position, direction, obstacle_positions)
    )
    visited: set[Coordinate] = set()
    visited_twice: set[Coordinate] = set()
    visited_thrice: set[Coordinate] = set()
    visited_fourth: set[Coordinate] = set()
    moved_last_step = True
    while True:
        if moved_last_step and guard_position in visited:
            if moved_last_step and guard_position in visited_twice:
                if moved_last_step and guard_position in visited_thrice:
                    if moved_last_step and guard_position in visited_fourth:
                        return True
                    visited_fourth.add(guard_position)
                visited_thrice.add(guard_position)
            visited_twice.add(guard_position)
        visited.add(guard_position)
        new_guard_position, guard_direction = get_next_guard_state(guard_position, guard_direction)
        if outside_bounds_calculator(new_guard_position):
            return False
        if new_guard_position != guard_position:
            moved_last_step = True
            guard_position = new_guard_position
        else:
            moved_last_step = False


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
    assert result == 6

    start_time = time.time()
    result = main(input_as_lines())
    print(f"Part one result:\n{result}")
    assert result < 1567
    print(f"Time taken: {time.time() - start_time:.3f} seconds")
