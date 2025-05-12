from collections import deque

from file_handling import get_file_as_lines

from day9.utils import get_information


def main(lines: list[str]) -> int:

    data, holes = get_information(lines)

    calculate_next_count = next_count_calculator()

    filled_data = move_data_into_holes(data, holes)

    count = sum(calculate_next_count(next_number) for next_number in filled_data)
    # count = sum(map(calculate_next_count, filled_data))
    return count


def move_data_into_holes(og_data, og_holes):
    data = deque(og_data)
    holes = deque(og_holes)
    return_data = [data.popleft()]

    while data:
        filled_hole, new_hole, new_end_data = fill_next_hole(holes.popleft(), data.pop())
        return_data.append(filled_hole)
        if new_hole is None:
            if data:
                return_data.append(data.popleft())
        if new_hole is not None:
            holes.appendleft(new_hole)
        if new_end_data is not None:
            data.append(new_end_data)
    return return_data



def fill_next_hole(hole_length, end_data):
    """
    :param end_data:
    :param hole_length:
    :return:
    Examples:
        >>> fill_next_hole(8,(5, 2))
        ((5, 2), 3, None)
        >>> fill_next_hole(5,(5, 2))
        ((5, 2), None, None)
        >>> fill_next_hole(4,(5, 2))
        ((4, 2), None, (1, 2))
    """
    end_data_length = end_data[0]
    end_data_count = end_data[1]

    new_hole_length = hole_length - end_data_length
    new_end_data_length = end_data_length - hole_length
    new_filled_hole_length = min(end_data_length, hole_length)

    new_hole = new_hole_length if new_hole_length > 0 else None
    new_end_data = (new_end_data_length, end_data_count) if new_end_data_length > 0 else None
    filled_hole = (new_filled_hole_length, end_data_count)
    return filled_hole, new_hole, new_end_data


def next_count_calculator():
    index = 0
    def calc_next_count_closure(next_number):
        nonlocal index
        old_index  = index
        next_index = index + next_number[0]

        index = next_index

        return sum(i * next_number[1] for i in range(old_index, next_index))
    return calc_next_count_closure

if __name__ == '__main__':
    part_one_result = main(get_file_as_lines())
    # part_one_result = main(get_specific_file_as_lines('sample_input.txt'))
    print(f"Part one result:\n{part_one_result}")
    assert part_one_result == 6385338159127
    # assert part_one_result == 1928