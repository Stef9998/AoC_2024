from map_handling import Coordinate

def get_trail_starts(trail_map: list[list[int]]) -> set[Coordinate]:
    start_coordinates = set()
    for row_no, row in enumerate(trail_map):
        for column_no, cell in enumerate(row):
            if cell == 0:
                # TODO: x and y is swapped
                start_coordinates.add(Coordinate(row_no, column_no))
    return start_coordinates