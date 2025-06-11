from enum import Enum

from file_handling import input_as_lines, input_as_lines


class StripeColor(Enum):
    white = 'w'
    blue = 'u'
    black = 'b'
    red = 'r'
    green = 'g'


def parse_lines(lines: list[str]):
    """
    Parses the input lines to extract towel options and patterns.

    The first line contains towel options separated by commas.
    The second line is empty.
    The subsequent lines represent towel patterns.

    :param lines: A list of strings representing the input lines.
    :return: A tuple containing:
             - A dictionary mapping stripe colors to a list of tuples,
               where each tuple contains the length of the towel option and the towel option itself.
             - A list of towel patterns to build.
    """

    towel_options = lines[0].split(", ")
    towel_patterns = lines[2:]
    towel_option_dict: dict[str, list[tuple[int, str]]] = dict()
    _populate_dict = populate_dict_per_key(towel_options)
    for stripe_color in StripeColor:
        towel_option_dict[stripe_color.value] = _populate_dict(stripe_color)
    return towel_option_dict, towel_patterns


def populate_dict_per_key(towel_options: list[str]):
    """
    Creates a function to populate a dictionary for a given stripe color.

    The returned function filters towel options that start with the specified
    stripe color and maps them to tuples containing the length of the towel option
    and the towel option itself.

    :param towel_options: A list of towel options as strings.
    :return: A function that takes a StripeColor and returns a list of tuples
             containing the length of the towel option and the towel option itself.
    """

    def _populate_dict_per_key(stripe_color: StripeColor) -> list[tuple[int, str]]:
        color_begin_towels_options: filter[str] = filter(lambda towel_option: towel_option[0] == stripe_color.value, towel_options)
        return list(map(lambda towel_option: (len(towel_option), towel_option), color_begin_towels_options))

    return _populate_dict_per_key


if __name__ == '__main__':
    # parse_lines(input_as_lines("sample.txt"))
    parse_lines(input_as_lines())
