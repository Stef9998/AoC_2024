import list_handling
from day21.keypad import get_all_shortest_path


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


def get_possible_keys(move, pad_type):
    possible_paths = get_all_shortest_path(pad_type, move[0], move[1])
    possible_keys_to_press = [''.join((step.value for step in possible_path)) + 'A' for possible_path in possible_paths]
    return possible_keys_to_press


def movements_from_keys(d1_keys: str):
    return list(zip('A' + d1_keys, d1_keys))
