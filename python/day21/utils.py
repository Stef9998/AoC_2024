import list_handling


def parse_lines(lines: list[str]):
    return list_handling.parse_lines(lines, parser)

def parser(line: str):
    return list(zip('A'+line, line))

def get_number_from_numpad(keys: str):
    """

    :param keys:
    :return:

    >>> get_number_from_numpad("9999")
    Traceback (most recent call last):
        ...
    AssertionError
    >>> get_number_from_numpad("xxxA")
    Traceback (most recent call last):
        ...
    AssertionError: Not a valid integer: xxx
    >>> get_number_from_numpad("980A")
    980
    >>> get_number_from_numpad("029A")
    29
    """
    assert keys[-1] == 'A'
    number = keys[:-1]
    assert number.isdigit(), f"Not a valid integer: {number}"
    return int(number)