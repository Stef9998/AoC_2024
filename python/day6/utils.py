from map_handling import Coordinate, Direction

def turn_right(direction: Direction) -> Direction:
    directions = [Direction.UP, Direction.RIGHT, Direction.DOWN, Direction.LEFT]
    current_index = directions.index(direction)
    return directions[(current_index + 1) % len(directions)]


def init_maze_findings(maze: list[str]):
    width = len(maze[0])
    height = len(maze)
    guard_start = None
    guard_direction = None
    obstacle_positions: set[Coordinate] = set()
    for y in range(height):
        for x in range(width):
            char = maze[y][x]
            if is_empty(char):
                continue
            if is_obstacle(char):
                obstacle_positions.add(Coordinate(x, y))
                continue
            if is_direction(char):
                if guard_start is not None:
                    raise ValueError("Multiple start points found in the maze.")
                guard_start = Coordinate(x, y)
                guard_direction = get_direction(char)
                continue
            raise ValueError(f"Invalid character '{char}' at ({x}, {y})")

    if guard_start is None:
        raise ValueError("Start point not found in the maze.")

    return {
        "maze_width": width,
        "maze_height": height,
        "guard_start": guard_start,
        "guard_direction": guard_direction,
        "obstacle_positions": obstacle_positions,
    }


def is_correct_char(char: str) -> bool:
    return is_empty(char) or is_obstacle(char) or is_direction(char)


def is_empty(char: str) -> bool:
    return char == '.'


def is_obstacle(char: str) -> bool:
    return char == '#'


def is_direction(char: str) -> bool:
    return char in 'v^<>'


def get_direction(char: str) -> Direction:
    if char == 'v':
        return Direction.DOWN
    elif char == '^':
        return Direction.UP
    elif char == '<':
        return Direction.LEFT
    elif char == '>':
        return Direction.RIGHT
    else:
        raise ValueError(f"Invalid direction character: {char}")