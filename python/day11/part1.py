from file_handling import get_file_as_lines, get_specific_file_as_lines

from list_handling import number_parser


def main(lines: list[str]) -> int:
    numbers = number_parser(lines[0])

    for _ in range(25):
        numbers = blink(numbers)

    return len(numbers)


def blink(stones: list[int]):
    return [new_stone for stone in stones for new_stone in transform(stone)]


def transform(number: int):
    if number == 0:
        return (1,)
    if len(str(number)) % 2 == 0:
        return int(str(number)[:len(str(number)) // 2]), int(str(number)[len(str(number)) // 2:])
    return (number * 2024,)


if __name__ == '__main__':
    part_one_result = main(get_file_as_lines())
    # part_one_result = main(get_specific_file_as_lines('sample_input.txt'))
    print(f"Part one result:\n{part_one_result}")
