from day19.utils import parse_lines
from file_handling import input_as_lines, input_as_lines


def main(lines: list[str]) -> int:
    towel_options, towel_patterns = parse_lines(lines)

    return sum(recursion_solution(towel_options, towel_pattern) for towel_pattern in towel_patterns)


def recursion_solution(towel_options, towel_pattern) -> int:
    pattern_length = len(towel_pattern)

    from functools import lru_cache

    @lru_cache(maxsize=None)
    def recursion_step(pattern_index: int, towel_option):
        option_length = towel_option[0]
        if option_length > pattern_length - pattern_index:
            return False
        if towel_pattern[pattern_index:pattern_index + option_length] != towel_option[1]:
            return False
        new_index = pattern_index + option_length
        if new_index == pattern_length:
            return True
        return sum(recursion_step(new_index, towel_option) for towel_option in towel_options[towel_pattern[new_index]])

    return sum(recursion_step(0, towel_option) for towel_option in towel_options[towel_pattern[0]])


if __name__ == '__main__':
    import time

    # result = main(input_as_lines('sample.txt'))
    # print(f"Sample result:\n{result}")
    # assert result == 16

    start_time = time.time()
    result = main(input_as_lines())
    print(f"Part two result:\n{result}")
    assert result == 1016700771200474
    print(f"Time taken: {time.time() - start_time:.2f} seconds")
