def main(lines: list[str]) -> int:
    return sum(found_x_patterns_in_specific_line(lines, line_number) for line_number in range(1, len(lines) - 1))


def found_x_patterns_in_specific_line(lines: list[str], line_number: int) -> int:
    if line_number < 1 or line_number > len(lines) - 2:
        raise ValueError("Line number out of range")
    return found_x_patterns_in_line(lines[line_number - 1], lines[line_number], lines[line_number + 1])


def found_x_patterns_in_line(previous_line, current_line, next_line):
    return sum(1
               for char_index, char in enumerate(current_line[1:-1], start=1)
               if is_center_of_x_pattern(char, char_index, previous_line, next_line))


def is_center_of_x_pattern(char, char_index, previous_line, next_line):
    do_continue = True
    if char != 'A':
        do_continue = False
    if not are_opposite_x_arms(previous_line[char_index - 1], next_line[char_index + 1]):
        do_continue = False
    if not are_opposite_x_arms(previous_line[char_index + 1], next_line[char_index - 1]):
        do_continue = False
    return do_continue


def are_opposite_x_arms(first_char: str, second_char: str) -> bool:
    if first_char != 'M' and first_char != 'S':
        return False
    if second_char != 'M' and second_char != 'S':
        return False
    if second_char == first_char:
        return False
    return True
