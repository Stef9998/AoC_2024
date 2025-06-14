from day5.utils import middle_number, ordering_violation_calculator, parse
from file_handling import input_as_lines


def main(lines: list[str]) -> int:
    orderings, printing = parse(lines)

    is_right_order = ordering_violation_calculator(list(orderings))
    return sum(map(middle_number, filter(is_right_order, printing)))
    # is_wrong_order = lambda x: not is_right_order(x)
    # return sum(middle_number(pages) for pages in printing if not is_wrong_order(pages))


if __name__ == '__main__':
    import time

    result = main(input_as_lines('sample.txt'))
    print(f"Sample one result:\n{result}")
    assert result == 143

    start_time = time.time()
    result = main(input_as_lines())
    print(f"Part one result:\n{result}")
    assert result == 6505
    print(f"Time taken: {time.time() - start_time:.3f} seconds")
