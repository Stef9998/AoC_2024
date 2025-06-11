from collections.abc import Iterator

from python.day13.utils import get_input_as_blocks, button_b_cost, button_a_cost
from python.file_handling import input_as_lines, input_as_lines
from python.map_handling import Coordinate


def main(lines: list[str]) -> int:
    data_blocks_p1 = get_input_as_blocks(lines)
    data_blocks_corrected = correct_unit_conversion(data_blocks_p1)

    # return sum(do_calc_analytic(*data_block) for data_block in data_blocks_corrected)
    return sum(do_calc_analytic_2(data_blocks_corrected))


def do_calc_analytic_2(list_iterator: Iterator[tuple[Coordinate, Coordinate, Coordinate]]) -> Iterator[int]:
    button_presses = do_calc_iterator(list_iterator)
    return map(get_costs_of_button_presses, button_presses)


def get_costs_of_button_presses(button_presses):
    button_a_presses, button_b_presses = button_presses
    return button_a_presses * button_a_cost + button_b_presses * button_b_cost


def do_calc_iterator(list_iterator: Iterator[tuple[Coordinate, Coordinate, Coordinate]]) -> Iterator[tuple[int, int]]:
    button_presses = (get_button_presses_all(button_a_movement, button_b_movement, prize) for
                      button_a_movement, button_b_movement, prize in list_iterator)
    button_presses_that_are_int = filter(presses_are_integer, button_presses)
    int_button_presses = map(float_tuple_to_int, button_presses_that_are_int)
    return int_button_presses


def float_tuple_to_int(button_presses):
    button_a_presses, button_b_presses = button_presses
    return int(button_a_presses), int(button_b_presses)


def presses_are_integer(button_presses: tuple[float, float]) -> bool:
    button_a_presses, button_b_presses = button_presses
    return button_a_presses.is_integer() and button_b_presses.is_integer()


def get_button_presses_all(button_a_movement, button_b_movement, prize) -> tuple[float, float]:
    button_a_presses = (prize.x * button_b_movement.y - prize.y * button_b_movement.x) / (
            button_a_movement.x * button_b_movement.y - button_a_movement.y * button_b_movement.x)
    button_b_presses = (prize.y * button_a_movement.x - prize.x * button_a_movement.y) / (
            button_a_movement.x * button_b_movement.y - button_a_movement.y * button_b_movement.x)
    return button_a_presses, button_b_presses


def do_calc_analytic(button_a_movement: Coordinate, button_b_movement: Coordinate, prize: Coordinate) -> int:
    button_a_presses, button_b_presses = get_button_presses_filter_not_possible(button_a_movement, button_b_movement, prize)
    return button_a_presses * button_a_cost + button_b_presses * button_b_cost


def get_button_presses_filter_not_possible(button_a_movement, button_b_movement, prize):
    button_a_presses, button_b_presses = get_button_presses_all(button_a_movement, button_b_movement, prize)

    if not button_a_presses.is_integer() or not button_b_presses.is_integer():
        return 0, 0

    return int(button_a_presses), int(button_b_presses)


def correct_unit_conversion(data: list[tuple[Coordinate, Coordinate, Coordinate]]) -> Iterator[tuple[Coordinate, Coordinate, Coordinate]]:
    return ((button_a, button_b, Coordinate(prize.x + 10000000000000, prize.y + 10000000000000)) for
            button_a, button_b, prize in data)


if __name__ == '__main__':
    result = main(input_as_lines())
    # result = main(input_as_lines('sample.txt'))
    print(f"Part two result:\n{result}")
    assert result == 79352015273424
