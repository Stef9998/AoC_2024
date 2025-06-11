from map_handling import Coordinate, out_of_bounds


def init_map_findings(maze: list[str]):
    width = len(maze[0])
    height = len(maze)
    findings = {}
    for y in range(height):
        for x in range(width):
            char = maze[y][x]
            if char == '.':
                continue
            if char.isdigit() or char.isalpha():
                add_char_to_database(char, x, y, findings)
            else:
                raise ValueError(f"Invalid character '{char}' at ({x}, {y})")
    return findings, maze


def add_char_to_database(char: str, x: int, y: int, database: dict[str, list[Coordinate]]):
    if char not in database:
        database[char] = []
    database[char].append(Coordinate(x, y))


def init_in_bounds_calculator(width: int, height: int):
    def is_in_bounds(coordinate: Coordinate):
        return not out_of_bounds(Coordinate(*coordinate), width, height)

    return is_in_bounds
