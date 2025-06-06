import itertools
from collections.abc import Iterator
from functools import lru_cache

from day22.utils import next_secret_number
from file_handling import get_file_as_lines, get_specific_file_as_lines


def main(lines: list[str]) -> int:
    buyers = []
    for buyer_secret_number in lines:
        buyers.append(one_buyer(int(buyer_secret_number)))
    for change_sequence in possible_change_sequence():
        print(change_sequence)

        print(sum(map(lambda buyer: buyer.get(change_sequence, 0), buyers)))

        if change_sequence == (-2, 1, -1, 3):
            exit(0)

    return 0


def possible_change_sequence() -> Iterator[tuple[int, int, int, int]]:
    for _1 in range(-9, 10):
        movement_1 = _1
        range_low_1 = max(-9, -9 - movement_1)
        range_height_1 = min(10, 10 - movement_1)
        for _2 in range(range_low_1, range_height_1):
            movement_2 = _1 + _2
            range_low_2 = max(-9, -9 - movement_2)
            range_height_2 = min(10, 10 - movement_2)
            for _3 in range(range_low_2, range_height_2):
                movement_3 = _1 + _2 + _3
                range_low_3 = max(-9, -9 - movement_3)
                range_height_3 = min(10, 10 - movement_3)
                for _4 in range(range_low_3, range_height_3):
                    yield _1, _2, _3, _4
    return


def possible_change_sequence_() -> Iterator[tuple[int, int, int, int]]:
    for _1 in range(-9, 10):
        movement_1 = _1
        for _2 in range(*range_calc(movement_1)):
            movement_2 = _1 + _2
            for _3 in range(*range_calc(movement_2)):
                movement_3 = _1 + _2 + _3
                for _4 in range(*range_calc(movement_3)):
                    yield _1, _2, _3, _4
    return


def possible_change_sequence__() -> Iterator[tuple[int, int, int, int]]:
    for _1 in range(-9, 10):
        for _2 in range(*range_calc(_1)):
            for _3 in range(*range_calc(_1 + _2)):
                for _4 in range(*range_calc(_1 + _2 + _3)):
                    yield _1, _2, _3, _4
    return


def range_calc(current_movement: int) -> tuple[int, int]:
    return (max(-9, -9 - current_movement)), (min(10, 10 - current_movement))


def one_buyer(secret_number: int):
    secrets = itertools.chain([secret_number], next_secret_numbers_gen(2000, secret_number))
    banana_counts_1, banana_counts_2 = itertools.tee(map(get_ones_digit, secrets), 2)
    changes = get_changes_gen(banana_counts_1)
    sequences = four_changes_gen(changes)
    zipped = zip(sequences, itertools.islice(banana_counts_2, 4, None))
    sequence_dict = {}
    for seq, banana_count in zipped:
        if seq not in sequence_dict:
            sequence_dict[seq] = banana_count

    return sequence_dict


def zipped_gen(sequences: Iterator[tuple[int, int, int, int]], banana_count: Iterator[int]) -> \
        Iterator[tuple[tuple[int, int, int, int], int]]:
    while True:
        try:
            yield next(sequences), next(banana_count)
        except StopIteration:
            return


def four_changes(changes: list[int]):
    return zip(changes, changes[1:], changes[2:], changes[3:])


def four_changes_gen(changes: Iterator[int]) -> Iterator[tuple[int, int, int, int]]:
    elem_1: int
    elem_2 = next(changes)
    elem_3 = next(changes)
    elem_4 = next(changes)
    while True:
        try:
            elem_1, elem_2, elem_3 = elem_2, elem_3, elem_4
            elem_4 = next(changes)
            yield elem_1, elem_2, elem_3, elem_4
        except StopIteration:
            return


def get_changes_gen(secret_numbers: Iterator[int]) -> Iterator[int]:
    elem_1: int
    elem_2 = next(secret_numbers)
    while True:
        try:
            elem_1 = elem_2
            elem_2 = next(secret_numbers)
            yield elem_2 - elem_1
        except StopIteration:
            return


def get_changes(secret_numbers: list[int]) -> Iterator[int]:
    return (b - a for a, b in zip(secret_numbers, secret_numbers[1:]))


def next_secret_numbers_gen(count: int, secret_number: int):
    current = secret_number
    for _ in range(count):
        current = next_secret_number(current)
        yield current


def next_secret_numbers(count: int, secret_number: int) -> list[int]:
    differences = []
    current = secret_number
    for _ in range(count):
        next_num = next_secret_number(current)
        differences.append(next_num)
        current = next_num
    return differences


def get_ones_digit(number: int) -> int:
    return number % 10


def benchmark():
    iterations = 1_000
    start_time = time.time()
    collect = []
    for _ in range(iterations):
        foo = list(possible_change_sequence())
        collect.append(foo)
    print(f"Time taken: {time.time() - start_time:.2f} seconds")
    start_time = time.time()
    collect = []
    for _ in range(iterations):
        foo = list(possible_change_sequence_())
        collect.append(foo)
    print(f"Time taken: {time.time() - start_time:.2f} seconds")
    start_time = time.time()
    collect = []
    for _ in range(iterations):
        foo = list(possible_change_sequence__())
        collect.append(foo)
    print(f"Time taken: {time.time() - start_time:.2f} seconds")


if __name__ == '__main__':
    import time

    # benchmark()
    # exit(0)

    # foo = one_buyer(123)
    # print(foo)
    # exit(0)

    result = main(get_specific_file_as_lines('sample_input.txt'))
    print(f"Sample result:\n{result}")
    # assert result ==
    exit(0)

    start_time = time.time()
    result = main(get_file_as_lines())
    print(f"Part two result:\n{result}")
    # assert result ==
    print(f"Time taken: {time.time() - start_time:.2f} seconds")
