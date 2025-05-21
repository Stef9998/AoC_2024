import sys

from python.day13.utils import split_into_blocks, get_input_as_blocks
from python.file_handling import get_file_as_lines
from python.map_handling import Coordinate

button_a_cost = 3
button_b_cost = 1

def main(lines: list[str]) -> int:

    data_blocks = get_input_as_blocks(lines)

    return sum(get_lowest_cost(button_a, button_b, prize) for button_a, button_b, prize in data_blocks)


def get_lowest_cost(button_a_movement: Coordinate, button_b_movement: Coordinate, prize: Coordinate) -> int:
    """
    Calculates the lowest cost to reach the prize from the button positions.

    :param button_a_movement: Movement of button A.
    :param button_b_movement: Movement of button B.
    :param prize: Position of the prize.
    :return: The lowest cost to reach the prize.

    >>> from python.map_handling import Coordinate
    >>> get_lowest_cost(Coordinate(94, 34), Coordinate(22, 67), Coordinate(8400, 5400))
    280
    """
    find_lowest_cost = find_lowest_cost_calculator(button_a_movement, button_b_movement, prize)
    lowest_cost = find_lowest_cost(Coordinate(0, 0), 0, 0, 0)
    if lowest_cost == sys.maxsize:
        return 0
    return lowest_cost


def find_lowest_cost_calculator(button_a_movement: Coordinate, button_b_movement: Coordinate, prize: Coordinate):
    # TODO found cost might not be given down as parameter, but might be enough to just do a min when getting from the recursive call
    def find_lowest_cost_recursive(current_coordinate: Coordinate , button_a_pressed_count: int, button_b_pressed_count: int, current_cost: int):
        # print(f"Current coordinate: {current_coordinate}, Button A pressed: {button_a_pressed_count}, Button B pressed: {button_b_pressed_count}, Current cost: {current_cost}, Found cost: {found_cost}")
        if button_a_pressed_count >= 100 or button_b_pressed_count >= 100:
            return sys.maxsize
        if current_coordinate.x > prize.x or current_coordinate.y > prize.y:
            return sys.maxsize
        if current_coordinate == prize:
            return current_cost
        if button_b_pressed_count == 0:
            after_a_found_cost = find_lowest_cost_recursive(current_coordinate + button_a_movement,
                                                        button_a_pressed_count + 1, button_b_pressed_count,
                                                        current_cost + button_a_cost)
            after_b_found_cost = find_lowest_cost_recursive(current_coordinate + button_b_movement,
                                                            button_a_pressed_count,
                                                            button_b_pressed_count + 1, current_cost + button_b_cost)
            return min(after_a_found_cost, after_b_found_cost)

        after_b_found_cost = find_lowest_cost_recursive(current_coordinate + button_b_movement, button_a_pressed_count,
                                                        button_b_pressed_count + 1, current_cost + button_b_cost)
        return after_b_found_cost

    return find_lowest_cost_recursive


if __name__ == '__main__':
    result = main(get_file_as_lines())
    # result = main(get_specific_file_as_lines('sample_input.txt'))
    print(f"Part one result:\n{result}")
    # assert result ==
