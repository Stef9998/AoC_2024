from file_handling import get_file_as_lines, get_specific_file_as_lines

from utils import get_numbers

import time

global buffer

def main(lines: list[str]) -> int:
    global buffer
    buffer = dict()

    numbers = get_numbers(lines[0])

    begin_time = time.time()
    for i in range(40):
        start_time = time.time()
        numbers = blink(numbers)
        print(f"Iteration {i}, time: {start_time - begin_time:.3f}s")
    print(f"Time: {time.time() - begin_time:.3f}s")

    return len(numbers)


def blink(stones: list[int]):
    return [new_stone for stone in stones for new_stone in transform(stone)]


def transform(number: int):
    global buffer
    if number in buffer:
        return buffer[number]
    if number == 0:
        buffer[number] = (1,)
        return (1,)
    if len(str(number)) % 2 == 0:
        new_numbers = int(str(number)[:len(str(number)) // 2]), int(str(number)[len(str(number)) // 2:])
        buffer[number] = new_numbers
        return new_numbers
    buffer[number] = (number * 2024,)
    return (number * 2024,)


if __name__ == '__main__':
    part_two_result = main(get_file_as_lines())
    # part_one_result = main(get_specific_file_as_lines('sample_input.txt'))
    print(f"Part two result:\n{part_two_result}")
    assert part_two_result == 1604873