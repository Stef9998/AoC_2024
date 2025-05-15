
def get_trail_starts(trail_map: list[list[int]]) -> set[(int, int)]:
    start_coordinates = set()
    for row_no, row in enumerate(trail_map):
        for column_no, cell in enumerate(row):
            if cell == 0:
                start_coordinates.add((row_no, column_no))
    return start_coordinates