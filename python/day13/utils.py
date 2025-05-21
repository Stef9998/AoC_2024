from python.map_handling import Coordinate


def get_input_as_blocks(lines: list[str]) -> list[tuple[Coordinate, Coordinate, Coordinate]]:
    """
    Extracts data blocks from the input lines in the specified format.

    :param lines: List of input lines.
    :return: List of dictionaries containing extracted data for each block.
    """
    blocks = split_into_blocks(lines)

    extracted_data = []
    for block in blocks:
        data_strings = lines_to_data_strings(block)
        data = data_strings_to_data(data_strings)
        extracted_data.append(data)

    return extracted_data


def data_strings_to_data(block) -> tuple[Coordinate, Coordinate, Coordinate]:

    data1 = tuple_data_str_to_int(block[0], remove_prefix_button)
    data2 = tuple_data_str_to_int(block[1], remove_prefix_button)
    data3 = tuple_data_str_to_int(block[2], remove_prefix_prize)

    return data1, data2, data3


def tuple_data_str_to_int(movement_input: tuple[str, str], prefix_remover):
    movement = prefix_remover(movement_input)
    return tuple_string_to_coordinate(movement)


def tuple_string_to_coordinate(movement) -> Coordinate:
    return Coordinate(int(movement[0]), int(movement[1]))


def remove_prefix_prize(movement_input):
    return movement_input[0].removeprefix("X="), movement_input[1].removeprefix("Y=")


def remove_prefix_button(movement_input):
    return movement_input[0].removeprefix("X"), movement_input[1].removeprefix("Y")


def lines_to_data_strings(block) -> tuple:
    data = [0,0,0]
    if not is_block_right_format(block):
        raise ValueError(f"Invalid block format: {block}")
    x, y = block[0].split(":")[1].strip().split(", ")
    data[0] = (x, y)
    x, y = block[1].split(":")[1].strip().split(", ")
    data[1] = (x, y)
    x, y = block[2].split(":")[1].strip().split(", ")
    data[2] = (x, y)
    return tuple(data)


def is_block_right_format(block):
    if not block[0].startswith("Button A:"):
        raise ValueError(f"Invalid line 1 format: {block[0]}")
    if not block[1].startswith("Button B:"):
        raise ValueError(f"Invalid line 2 format: {block[1]}")
    if not block[2].startswith("Prize:"):
        raise ValueError(f"Invalid line 3 format: {block[2]}")
    return True

def split_into_blocks(lines: list[str]) -> list[tuple[str, str, str]]:
    """
    Splits the input lines into blocks of three lines each.

    :param lines: List of input lines.
    :return: List of blocks, where each block is a tuple of three lines.
    """
    non_empty_lines = [line.strip() for line in lines if line.strip()]
    return [(non_empty_lines[i], non_empty_lines[i+1], non_empty_lines[i+2]) for i in range(0, len(non_empty_lines), 3)]

