from utils import init_map_findings, init_out_of_bounds_calculator

from file_handling import get_file_as_lines

def main (lines: list[str]) -> int:
    map_findings, maze = init_map_findings(lines)
    out_of_bounds_calculator = init_out_of_bounds_calculator(len(maze[0]), len(maze))
    print(map_findings)

    antinodes = set()
    for specific_frequency_antennas in map_findings.values():
        antinodes.update( find_antinodes(specific_frequency_antennas, out_of_bounds_calculator) )
    print(antinodes)
    return len(antinodes)


def find_antinodes(antenna_coordinates: list[(int,int)], is_out_of_bounds):
    antinode_coordinates = set()
    for i, base_antenna_coordinate in enumerate(antenna_coordinates):
        for antenna_coordinate in antenna_coordinates[i+1:]:
            diff = (base_antenna_coordinate[0] - antenna_coordinate[0], base_antenna_coordinate[1] - antenna_coordinate[1])

            possible_antinode_antenna_1 = (base_antenna_coordinate[0] + diff[0], base_antenna_coordinate[1] + diff[1])
            possible_antinode_antenna_2 = (antenna_coordinate[0] - diff[0], antenna_coordinate[1] - diff[1])

            if is_out_of_bounds(possible_antinode_antenna_1):
                antinode_coordinates.add(possible_antinode_antenna_1)
            if is_out_of_bounds(possible_antinode_antenna_2):
                antinode_coordinates.add(possible_antinode_antenna_2)
    return antinode_coordinates


if __name__ == '__main__':
    part_one_result = main(get_file_as_lines())
    print(f"Part one result:\n{part_one_result}")
