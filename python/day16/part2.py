from day16.part1 import get_next_possible_directions, get_tile, get_next_turn_direction, is_straight_no_branch, \
    branch_factor, get_start_directions
from day16.utils import Tile, get_map_information, print_map, can_step_on_tile
from file_handling import get_file_as_lines, get_specific_file_as_lines
from map_handling import Coordinate, Direction

global lowest_found_path_glob

# TODO:
# cache something to not needing to calculate again
#  maybe lowest at branch coordinate
#   might be not correct, as another path through this is needed later
#   need to check
#  maybe lowest to end
#   same question as before
# when returning shortest path found in recursion. If at one point my current path-length is longer instantly backtrack, as I can't make it better

def main(lines: list[str], given_shortest_path: int = None) -> int:
    import day16.part1
    shortest_path_length = given_shortest_path if given_shortest_path is not None else day16.part1.main(lines)

    height, width, start_coordinate = get_map_information(lines)

    return len(all_shortest_paths_tiles(lines, shortest_path_length, start_coordinate))


def all_shortest_paths_tiles(map_data, lowest_found_path, start_coordinate):
    all_visited_in_shortest_paths = set()

    def start_finding_for_all_directions():
        start_directions = get_start_directions(map_data, start_coordinate)
        for start_direction in start_directions:
            start_recursion(start_direction)
        return all_visited_in_shortest_paths

    def start_recursion(start_direction):
        if start_direction == Direction.RIGHT:
            start_score = 0
        elif start_direction == Direction.LEFT:
            start_score = 2000
        else:
            start_score = 1000
        recursive_find_path(start_coordinate, start_direction, {start_coordinate}, start_score, dict())

    def recursive_find_path(coordinate: Coordinate, direction: Direction, visited: set[Coordinate],
                            path_length, branch_lowest):
        """

        :param visited:
        :param branch_lowest:
        :param coordinate:
        :param direction:
        :param path_length:

        :return: the shortest found path

        """
        nonlocal all_visited_in_shortest_paths
        nonlocal lowest_found_path
        if get_tile(coordinate, map_data) is Tile.END:
            if path_length == lowest_found_path:
                all_visited_in_shortest_paths.update(visited)
            return
        current_coordinate = coordinate + direction.value
        current_visited = visited.union({current_coordinate})
        current_path_length = path_length + 1
        current_direction = direction

        while branch_factor(current_coordinate, current_direction, map_data, visited) == 1:
            if get_tile(current_coordinate, map_data) is Tile.END:
                if current_path_length == lowest_found_path:
                    all_visited_in_shortest_paths.update(current_visited)
                return
            if is_straight_no_branch(current_coordinate, current_direction, map_data, visited):
                current_coordinate += current_direction.value
                current_visited.add(current_coordinate)
                current_path_length += 1
            else:
                current_direction = get_next_turn_direction(current_coordinate, current_direction, map_data, visited)
                current_coordinate += current_direction.value
                current_visited.add(current_coordinate)
                current_path_length += 1001
            if get_tile(current_coordinate, map_data) is Tile.END:
                if current_path_length == lowest_found_path:
                    all_visited_in_shortest_paths.update(current_visited)
                return
        if current_coordinate in branch_lowest and branch_lowest[current_coordinate] + 1005 < current_path_length:
            return
        if can_step_on_tile(current_coordinate + current_direction.value, map_data, visited):
            branch_lowest[current_coordinate] = current_path_length
        elif current_coordinate in branch_lowest and branch_lowest[current_coordinate] > current_path_length + 1000:
            branch_lowest[current_coordinate] = current_path_length + 1000
        for new_direction in get_next_possible_directions(current_coordinate, current_direction, map_data, visited):
            # print_map(map_data, current_coordinate, new_direction, all_visited_tiles)
            if current_path_length > lowest_found_path:
                return
            if new_direction is not current_direction:
                if current_path_length + 1000 > lowest_found_path:
                    return
                recursive_find_path(current_coordinate, new_direction, current_visited,
                                    current_path_length + 1000, branch_lowest)
            else:
                recursive_find_path(current_coordinate, new_direction, current_visited,
                                    current_path_length, branch_lowest)
        return

    return start_finding_for_all_directions()

if __name__ == '__main__':

    import time

    result = main(get_specific_file_as_lines('sample_input.txt'), 7036)
    print(f"Sample one result:\n{result}")
    assert result == 45
    result = main(get_specific_file_as_lines('sample2_input.txt'), 11048)
    print(f"Sample two result:\n{result}")
    assert result == 64

    start_time = time.time()
    result = main(get_file_as_lines(), 143580)
    print(f"Part one result:\n{result}")
    assert result > 629
    assert result == 645
    print(f"Time taken: {time.time() - start_time:.2f} seconds")
