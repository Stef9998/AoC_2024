from typing import Iterable

from day7.utils import lines_to_data
from file_handling import get_file_as_lines


def add(x, y):
    return x + y

def multiply(x, y):
    return x * y

def concat(x, y):
    return int(str(x) + str(y))

operator_functions_p1 = [
    add,
    multiply,
]

operator_functions_p2 = [
    add,
    multiply,
    concat,
]

operator_functions = []


# modifying a global variable is ugly, but this is just, because I want to be able to run part1 and part2
# from this one file.
def part_one(lines):
    global operator_functions
    operator_functions = operator_functions_p1
    return main(lines)


def part_two(lines):
    global operator_functions
    operator_functions = operator_functions_p2
    return main(lines)


def main(lines: list[str]) -> int:

    data_lines = lines_to_data(lines)

    return calculate_day7_func(data_lines)


def calculate_day7_proc(data_lines):
    found_results = 0
    for data in data_lines:
        found = is_result_calculable(*data)
        if found:
            found_results += data[0]

    return found_results


def calculate_day7_func(data_lines):
    return sum(data[0] for data in data_lines if is_result_calculable(*data))


def is_result_calculable(result, operants: Iterable[int]):

    try:
        next_operant = next(operants)
    except StopIteration:
        return False

    operants_list = list(operants)
    # print(f"{[next_operant] + operants_list}")
    return result_calculator_recursive(result, next_operant, operants_list, 0)


# I like the more procedural calculation with a for loop and an if to break early more,
# than the more functional approach with an any(func() for ...)
def result_calculator_recursive(result, accumulator, operants, operants_index):

    if operants_index == len(operants):
        return result == accumulator

    # recursive calculation "procedural"
    for operator_function in operator_functions:
        new_accumulator = operator_function(accumulator, operants[operants_index])
        # print(f"Acc: {accumulator:<4}\tOperator: {operator_function.__name__:>8}\tNew_Acc: {new_accumulator}\tIs Equal Result: {new_accumulator == result}")
        if result_calculator_recursive(result, new_accumulator, operants.copy(), operants_index + 1):
            return True

    return False

    # # recursive calculation "functional"
    # return any(
    #     result_calculator_recursive(
    #         result,
    #         operator_function(accumulator, operants[operants_index]),
    #         operants.copy(),
    #         operants_index + 1
    #     )
    #     for operator_function in operator_functions
    # )

part_one_target = 850435817339
part_two_target = 104824810233437

if __name__ == '__main__':
    part_one_result = part_one(get_file_as_lines())
    print(f"Part one result:\n{part_one_result}")
    assert part_one_result == part_one_target, f'Expected {part_one_target}, but got {part_one_result}'
    part_two_result = part_two(get_file_as_lines())
    print(f"Part two result:\n{part_two_result}")
    assert part_two_result == part_two_target, f'Expected {part_two_target}, but got {part_two_result}'