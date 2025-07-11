from file_handling import input_as_lines

from list_handling import number_parser

global buffer


def main(lines: list[str]) -> int:
    global buffer
    buffer = dict()

    numbers = number_parser(lines[0])

    value = sum(transform_rec2(number, 75) for number in numbers)

    return value


def transform_rec2(number: int, blinks: int) -> int:
    global buffer
    if (number, blinks) in buffer:
        return buffer[(number, blinks)]
    number_len = len(str(number))
    next_blinks = blinks - 1
    if blinks == 0:
        return 1
    if number == 0:
        temp = transform_rec2(1, next_blinks)
        buffer[(number, blinks)] = temp
        return temp
    if number_len % 2 == 0:
        temp = transform_rec2(int(str(number)[:len(str(number)) >> 1]), next_blinks) + transform_rec2(
            int(str(number)[len(str(number)) >> 1:]), next_blinks)
        buffer[(number, blinks)] = temp
        return temp

    return transform_rec2(number * 2024, next_blinks)


if __name__ == '__main__':
    part_two_result = main(input_as_lines())
    # part_one_result = main(input_as_lines('sample.txt'))
    print(f"Part two result:\n{part_two_result}")
