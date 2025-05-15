from itertools import count

from utils import init_map_findings, init_out_of_bounds_calculator

from file_handling import get_file_as_lines


def part_one(lines: list[str]) -> int:
    map_findings, maze = init_map_findings(lines)
    out_of_bounds_calculator = init_out_of_bounds_calculator(len(maze[0]), len(maze))

    antinodes = set()
    for specific_frequency_antennas in map_findings.values():
        antinodes.update(p1_find_antinodes(specific_frequency_antennas, out_of_bounds_calculator))
    # print(antinodes)
    return len(antinodes)


def part_two(lines: list[str]) -> int:
    map_findings, maze = init_map_findings(lines)
    out_of_bounds_calculator = init_out_of_bounds_calculator(len(maze[0]), len(maze))

    antinodes = set()
    for specific_frequency_antennas in map_findings.values():
        antinodes.update(p2_find_antinodes(specific_frequency_antennas, lambda antinode_antenna: not out_of_bounds_calculator(antinode_antenna)))
    # print(antinodes)
    return len(antinodes)


def p1_find_antinodes(antenna_coordinates: list[(int, int)], is_in_of_bounds):
    antinode_coordinates = set()
    for i, base_antenna_coordinate in enumerate(antenna_coordinates):
        possible_antinode_antennas_for_base_antenna = []
        for antenna_coordinate in antenna_coordinates[i+1:]:
            possible_antinode_antennas_for_base_antenna.extend(
                p1_possible_antinode_positions_calculator(antenna_coordinate, base_antenna_coordinate)
            )
        antinode_coordinates.update(filter(is_in_of_bounds, possible_antinode_antennas_for_base_antenna))
    return antinode_coordinates


def p1_possible_antinode_positions_calculator(antenna_coordinate, base_antenna_coordinate):
    diff = base_antenna_coordinate - antenna_coordinate
    possible_antinode_antenna_1 = base_antenna_coordinate + diff
    possible_antinode_antenna_2 = antenna_coordinate - diff
    return [possible_antinode_antenna_1, possible_antinode_antenna_2]


def p2_find_antinodes(antenna_coordinates: list[(int, int)], is_out_of_bounds):
    antinode_coordinates = set()
    for i, base_antenna_coordinate in enumerate(antenna_coordinates):
        for antenna_coordinate in antenna_coordinates[i+1:]:
            possible_antinode_antennas = p2_possible_antinode_positions_calculator(
                antenna_coordinate, base_antenna_coordinate, is_out_of_bounds)

            for possible_antinode_antenna in possible_antinode_antennas:
                antinode_coordinates.add(possible_antinode_antenna)
    return antinode_coordinates


def p2_possible_antinode_positions_calculator(antenna_coordinate, base_antenna_coordinate, is_out_of_bounds):
    diff = base_antenna_coordinate - antenna_coordinate
    possible_antinode_positions = []
    for i in count(start=0):
        possible_antinode_position = base_antenna_coordinate + i*diff
        if is_out_of_bounds(possible_antinode_position):
            break
        possible_antinode_positions.append(possible_antinode_position)
    for i in count(start=0):
        possible_antinode_position = antenna_coordinate - i*diff
        if is_out_of_bounds(possible_antinode_position):
            break
        possible_antinode_positions.append(possible_antinode_position)
    return possible_antinode_positions


if __name__ == '__main__':
    part_one_result = part_one(get_file_as_lines())
    print(f"Part one result:\n{part_one_result}")
    assert part_one_result == 318
    part_two_result = part_two(get_file_as_lines())
    print(f"Part one result:\n{part_two_result}")
    assert part_two_result == 1126