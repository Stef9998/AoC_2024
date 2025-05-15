from day6.utils import init_maze_findings

from map_handling import Coordinate


def main(lines: list[str]) -> int:

    maze_findings = init_maze_findings(lines)

    maze_width = maze_findings["maze_width"]
    maze_height = maze_findings["maze_height"]

    obstacle_positions = maze_findings["obstacle_positions"]
    guard_position = maze_findings["guard_start"]
    guard_direction = maze_findings["guard_direction"]

    visited: set[Coordinate] = set()

    while True:
        visited.add(guard_position)
        next_position = guard_position + guard_direction.value
        while next_position in obstacle_positions:
            guard_direction = guard_direction.turn_right()
            next_position = guard_position + guard_direction.value
        if next_position.x < 0 or next_position.x >= maze_width or next_position.y < 0 or next_position.y >= maze_height:
            break
        guard_position = next_position

    return len(visited)
