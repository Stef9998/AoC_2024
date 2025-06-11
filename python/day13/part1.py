import sys

from python.day13.utils import get_input_as_blocks, button_a_cost, button_b_cost
from python.file_handling import input_as_lines
from python.map_handling import Coordinate


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
    lowest_cost = find_lowest_cost()
    if lowest_cost == sys.maxsize:
        return 0
    return lowest_cost


def find_lowest_cost_calculator(button_a_movement: Coordinate, button_b_movement: Coordinate, prize: Coordinate):
    def find_lowest_cost_recursive(current_coordinate: Coordinate, button_b_pressed_count: int, current_cost: int):
        if current_coordinate == prize:
            return current_cost
        if button_b_pressed_count >= 100:
            return sys.maxsize
        if current_coordinate.x > prize.x or current_coordinate.y > prize.y:
            return sys.maxsize

        after_b_found_cost = find_lowest_cost_recursive(current_coordinate + button_b_movement,
                                                        button_b_pressed_count + 1,
                                                        current_cost + button_b_cost)
        if after_b_found_cost != sys.maxsize:
            return after_b_found_cost
        after_a_found_cost = button_a_recursive(current_coordinate + button_a_movement,
                                                1,
                                                current_cost + button_a_cost)
        return after_a_found_cost

    def button_a_recursive(current_coordinate: Coordinate, button_a_pressed_count: int, current_cost: int):
        if current_coordinate == prize:
            return current_cost
        if button_a_pressed_count >= 100:
            return sys.maxsize
        if current_coordinate.x > prize.x or current_coordinate.y > prize.y:
            return sys.maxsize

        after_a_found_cost = button_a_recursive(current_coordinate + button_a_movement,
                                                button_a_pressed_count + 1,
                                                current_cost + button_a_cost)
        return after_a_found_cost

    def find_lowest_cost_recursion_start():
        return find_lowest_cost_recursive(Coordinate(0, 0), 0, 0)

    return find_lowest_cost_recursion_start


if __name__ == '__main__':
    import time

    start_time = time.time()
    result = main(input_as_lines())
    # result = main(input_as_lines('sample.txt'))
    end_time = time.time()
    print(f"Part one result:\n{result}")
    print(f"Calculation time: {end_time - start_time:.4f} seconds")
    assert result == 36954
