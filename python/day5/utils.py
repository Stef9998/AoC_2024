def parse(lines: list[str]):
    orderings, print_pages = _get_datas(lines)
    orderings_ = map(_get_first_data, orderings)
    print_pages_ = map(_get_second_data, print_pages)
    return orderings_, print_pages_


def _get_first_data(line: str):
    return tuple(map(int, line.split('|')))


def _get_second_data(line: str):
    return list(map(int, line.split(',')))


def _get_datas(lines):
    empty_line_index = _get_empty_line_index(lines)
    return lines[:empty_line_index], lines[empty_line_index + 1:]


def _get_empty_line_index(lines):
    for i, line in enumerate(lines):
        if line == "":
            return i
    raise ValueError("No empty line")


def middle_number(numbers: list[int]) -> int:
    return numbers[len(numbers) // 2]


def ordering_violation_calculator(orderings: list[tuple[int, int]]):
    def test_pages(pages: list[int]):
        positions = {page: i for i, page in enumerate(pages)}

        def right_order(before: int, after: int) -> bool:
            return (
                    (before_index := positions.get(before)) is None
                    or (after_index := positions.get(after)) is None
                    or before_index < after_index
            )

        return all(right_order(before, after) for before, after in orderings)

    return test_pages
