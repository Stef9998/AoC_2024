from day5.utils import middle_number, ordering_violation_calculator, parse
from file_handling import input_as_lines


def main(lines: list[str]) -> int:
    orderings, printing = parse(lines)
    orderings_ = list(orderings)
    is_right_order = ordering_violation_calculator(orderings_)
    is_wrong_order = lambda _pages: not is_right_order(_pages)
    sort_pages = lambda _pages: make_right_order(orderings_, _pages)

    return sum(map(middle_number, map(sort_pages, filter(is_wrong_order, printing))))

def make_right_order(orderings, og_pages) -> list[int]:
    get_wrong_order = ordering_corrector(orderings)
    def bubbler(pages):
        if len(pages) <= 1:
            return pages
        new_pages = pages[1:] + [pages[0]]
        wrong_order = list(get_wrong_order(new_pages))
        if len(wrong_order) == 0:
            return new_pages
        positions = {page: i for i, page in enumerate(new_pages)}
        lowest_wrong_index = min(min(positions[before], positions[after]) for before, after in wrong_order)
        return new_pages[:lowest_wrong_index] + bubbler(new_pages[lowest_wrong_index:])

    def right_order(pages: list[int]):
        wrong_order = get_wrong_order(pages)
        positions = {page: i for i, page in enumerate(pages)}
        lowest_wrong_index = min(min(positions[before], positions[after]) for before, after in wrong_order)
        return pages[:lowest_wrong_index] + bubbler(pages[lowest_wrong_index:])

    return right_order(og_pages)



def ordering_corrector(orderings: list[tuple[int, int]]):
    def get_right_order(pages: list[int]):
        positions = {page: i for i, page in enumerate(pages)}

        def _right_order(before: int, after: int) -> bool:
            return (
                    (before_index := positions.get(before)) is None
                    or (after_index := positions.get(after)) is None
                    or before_index < after_index
            )

        return filter(lambda pair: not _right_order(*pair), orderings)

    return get_right_order


if __name__ == '__main__':
    import time

    result = main(input_as_lines('sample.txt'))
    print(f"Sample one result:\n{result}")
    assert result == 123

    start_time = time.time()
    result = main(input_as_lines())
    print(f"Part one result:\n{result}")
    # assert result ==
    print(f"Time taken: {time.time() - start_time:.3f} seconds")
