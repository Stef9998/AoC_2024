from day6.util import init_maze_findings, Coordinate


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
        next_position = (guard_position[0] + guard_direction.value[0], guard_position[1] + guard_direction.value[1])
        while next_position in obstacle_positions:
            guard_direction = guard_direction.turn_right()
            next_position = (guard_position[0] + guard_direction.value[0], guard_position[1] + guard_direction.value[1])
        if next_position[0] < 0 or next_position[0] >= maze_width or next_position[1] < 0 or next_position[1] >= maze_height:
            break
        guard_position = next_position

    return len(visited)
