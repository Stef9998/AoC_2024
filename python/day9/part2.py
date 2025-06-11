from python.file_handling import input_as_lines

from utils import get_more_information


def main(lines: list[str]) -> int:
    data, holes = get_more_information(lines)

    new_numbers = get_moved_data(data, holes)

    return sum(calc_next_count(number) for number in new_numbers)


def get_moved_data(og_data, og_holes):
    holes = og_holes
    new_numbers = []
    for og_number in reversed(og_data):
        holes, new_number = move_number_to_hole(holes, og_number)
        new_numbers.append(new_number)

    return new_numbers


def move_number_to_hole(holes: list, number):
    """
    Moves a number to the first hole it fits into.

    :param holes: A list of tuples representing holes, where each tuple contains:
                  - length: The length of the hole
                  - location: The location of the hole
    :param number: A tuple representing the number, where:
                   - length: The length of the number
                   - count: The global index of the number
                   - location: The location of the number in the global data-space
    :return: A tuple containing:
             - The updated list of holes
             - The updated number

    Examples:
        >>> move_number_to_hole([(5, 0), (9, 10), (8, 25)], (8, 2, 15))
        ([(5, 0), (1, 18), (8, 25)], (8, 2, 10))
        >>> move_number_to_hole([(5, 0), (9, 10), (8, 25)], (3, 2, 4))
        ([(2, 3), (9, 10), (8, 25)], (3, 2, 0))
        >>> move_number_to_hole([(5, 0), (9, 10), (8, 25)], (12, 2, 4))
        ([(5, 0), (9, 10), (8, 25)], (12, 2, 4))
    """
    new_number, hole_index, new_hole = get_moved_number_new_hole(holes, number)

    if new_hole is not None:
        modified_holes = [new_hole if i == hole_index else hole for i, hole in enumerate(holes)]
        return modified_holes, new_number

    # return copy.deepcopy(holes), new_number
    return holes, new_number


def get_moved_number_new_hole(holes, number):
    """
    :param holes:
    :param number:
    :return:

    Examples:
        >>> get_moved_number_new_hole([(5, 0), (9, 10), (8, 25)], (8, 2, 15))
        ((8, 2, 10), 1, (1, 18))
        >>> get_moved_number_new_hole([(5, 0), (9, 10), (8, 25)], (3, 2, 4))
        ((3, 2, 0), 0, (2, 3))
        >>> get_moved_number_new_hole([(5, 0), (9, 10), (8, 25)], (12, 2, 4))
        ((12, 2, 4), None, None)
    """
    number_length = number[0]
    number_location = number[2]
    for i, hole in enumerate(holes):
        # TODO refactor this into function
        hole_length = hole[0]
        hole_location = hole[1]
        if number_location < hole_location:
            break
        if number_length <= hole_length:
            new_hole_length = hole_length - number_length
            new_hole = (new_hole_length, hole_location + number_length)
            new_number = (number[0], number[1], hole_location)
            return new_number, i, new_hole
    return number, None, None


def calc_next_count(number):
    """

    :param number: A tuple (length, count, location) where:
                   - length: The length of the data-block
                   - count: The global index of the data-block
                   - location: The location of the data-block in the global data-space
    :return: The calculated sum

    Examples:
        >>> calc_next_count((3, 2, 4))
        30
        >>> calc_next_count((5, 1, 0))
        10
        >>> calc_next_count((2, 3, 7))
        45
    """
    location = number[2]
    next_location = number[2] + number[0]

    return sum(i * number[1] for i in range(location, next_location))


if __name__ == '__main__':
    part_two_result = main(input_as_lines())
    print(f"Part one result:\n{part_two_result}")
    assert part_two_result == 6415163624282
