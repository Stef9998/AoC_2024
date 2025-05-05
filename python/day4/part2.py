import collections
from collections import deque


def part_two(lines: list[str]) -> int:
    
    lines: deque[str] = collections.deque(lines)
    line_count: int = len(lines)

    match = 0

    previous_line = ""
    current_line = lines.popleft()
    next_line = lines.popleft()

    for _ in range(line_count - 2):
        previous_line = current_line
        current_line = next_line
        next_line = lines.popleft()

        for char_index, char in enumerate(current_line[1:-1], start=1):
            if char != 'A':
                continue
            if not are_opposite_x_arms(previous_line[char_index - 1], next_line[char_index + 1]):
                continue
            if not are_opposite_x_arms(previous_line[char_index + 1], next_line[char_index - 1]):
                continue
            match += 1

    return match


def are_opposite_x_arms(first_char: str, second_char: str) -> bool:
    if first_char != 'M' and first_char != 'S':
        return False
    if second_char != 'M' and second_char != 'S':
        return False
    if second_char == first_char:
        return False
    return True


if __name__ == '__main__':
    print(part_two(['MMMS',
                    'MAMS',
                    'SMSM',
                    'XAAS']))
