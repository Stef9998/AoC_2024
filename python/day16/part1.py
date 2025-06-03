import sys

from day16.utils import Tile, get_map_information, get_next_turn_direction, is_straight_no_branch, \
    branch_factor, get_start_directions, get_next_possible_directions, get_tile, print_map
from file_handling import get_file_as_lines, get_specific_file_as_lines
from map_handling import Coordinate, Direction


# TODO:
#  cache something to not needing to calculate again
#   maybe lowest at branch coordinate
#    might be not correct, as another path through this is needed later
#    need to check
#   maybe lowest to end
#    same question as before
#  when returning shortest path found in recursion. If at one point my current path-length is longer instantly backtrack, as I can't make it better

def main(lines: list[str]) -> int:
    height, width, start_coordinate = get_map_information(lines)

    find_shortest_path = recursive_closure(lines)
    _minimum = sys.maxsize
    start_directions = get_start_directions(lines, start_coordinate)
    for start_direction in start_directions:
        new_found_end_length = find_shortest_path(start_coordinate, start_direction)
        if new_found_end_length < _minimum:
            _minimum = new_found_end_length
    return _minimum


# TODO: def recursive
#  1.
#   list visited by order (to trackback)
#   set visited branch coordinates to test if already at some point at this to stop loop
#  2.
#   maybe save where this recursion step (recursion step is one line till branch) I started (easier backtrack)
#   still keep visited coordinates for branch
def recursive_closure(map_data):
    lowest_found_path = sys.maxsize

    def start_recursion(start_coordinate, start_direction):
        if start_direction == Direction.RIGHT:
            start_score = 0
        elif start_direction == Direction.LEFT:
            start_score = 2000
        else:
            start_score = 1000
        recursive_find(start_coordinate, start_direction, set(), start_score, dict())
        return lowest_found_path

    def recursive_find(coordinate: Coordinate, direction: Direction, visited: set[Coordinate], path_length,
                       branch_lowest):
        """

        :param branch_lowest:
        :param coordinate:
        :param direction:
        :param visited:
        :param path_length:

        :return: the shortest found path

        """
        nonlocal lowest_found_path
        if get_tile(coordinate, map_data) is Tile.END:
            lowest_found_path = min(lowest_found_path, path_length)
            return
        current_coordinate = coordinate + direction.value
        current_path_length = path_length + 1
        current_direction = direction
        current_visited = visited.union({current_coordinate})

        while branch_factor(current_coordinate, current_direction, map_data, current_visited) == 1:
            if get_tile(current_coordinate, map_data) is Tile.END:
                lowest_found_path = min(lowest_found_path, current_path_length)
                return
            if is_straight_no_branch(current_coordinate, current_direction, map_data, current_visited):
                current_coordinate += current_direction.value
                current_visited.add(current_coordinate)
                current_path_length += 1
            else:
                current_direction = get_next_turn_direction(current_coordinate, current_direction, map_data,
                                                            current_visited)
                current_coordinate += current_direction.value
                current_visited.add(current_coordinate)
                current_path_length += 1001
            if get_tile(current_coordinate, map_data) is Tile.END:
                lowest_found_path = min(lowest_found_path, current_path_length)
                return
        if current_coordinate in branch_lowest and branch_lowest[current_coordinate] <= current_path_length:
            return
        branch_lowest[current_coordinate] = current_path_length
        for new_direction in get_next_possible_directions(current_coordinate, current_direction, map_data,
                                                          current_visited):
            # print_map(map_data, current_coordinate, new_direction, current_visited)
            if current_path_length > lowest_found_path:
                return
            if new_direction is not current_direction:
                if current_path_length + 1000 > lowest_found_path:
                    return
                recursive_find(current_coordinate, new_direction, current_visited,
                                                      current_path_length + 1000, branch_lowest)
            else:
                recursive_find(current_coordinate, new_direction, current_visited,
                                                      current_path_length, branch_lowest)
        return

    return start_recursion


if __name__ == '__main__':
    import time

    result = main(get_specific_file_as_lines('sample_input.txt'))
    print(f"Sample one result:\n{result}")
    assert result == 7036
    result = main(get_specific_file_as_lines('sample2_input.txt'))
    print(f"Sample two result:\n{result}")
    assert result == 11048

    start_time = time.time()
    result = main(get_file_as_lines())
    print(f"Part one result:\n{result}")
    assert result == 143580
    print(f"Time taken: {time.time() - start_time:.2f} seconds")
