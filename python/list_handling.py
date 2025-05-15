from collections.abc import Iterator


def get_lines_of_numbers(lines: list[str]) -> Iterator[list[int]]:
    return parse_lines(lines, number_parser)


def parse_lines(lines: list[str], parser: callable) -> Iterator[list]:
    """
    Parses lines using the provided parser function.
    The parser function is for parsing a single line.

    :param lines: list of lines to parse. Lines are strings.
    :param parser: Parses a single line into an iterable.
    :return:
    """
    parse_as_list = lambda line: list(parser(line))
    return map(parse_as_list, lines)


def number_parser(line: str) -> Iterator[int]:
    number_strings = filter(lambda x: x.isdigit(), line.split())
    # number_strings = line.split()
    # numbers = map(lambda number: int(number), number_strings)
    numbers = map(int, number_strings)
    return numbers
