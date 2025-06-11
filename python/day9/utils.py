def get_information(lines: list[str]):
    line = lines[0]
    data = []
    empty = []
    for index, char in enumerate(line):
        if not char.isnumeric():
            raise ValueError("Invalid data provided to the function.\n"
                             "The chars need to be digits")
        if index % 2 == 0:
            data.append((int(char), index >> 1))
        else:
            # empty.append(((index-1)>>1,int(char)))
            empty.append(int(char))
    return data, empty


def get_more_information(lines: list[str]):
    line = lines[0]
    data = []
    holes = []
    location = 0
    for index, char in enumerate(line):
        if not char.isnumeric():
            raise ValueError("Invalid data provided to the function.\n"
                             "The chars need to be digits")
        char_value = int(char)
        if index % 2 == 0:
            data.append((char_value, index >> 1, location))
        else:
            holes.append((char_value, location))
        location += char_value
    return data, holes


def get_more_information_dict(lines: list[str]):
    line = lines[0]
    data = {}
    holes = {}
    location = 0
    for index, char in enumerate(line):
        if not char.isnumeric():
            raise ValueError("Invalid data provided to the function.\n"
                             "The chars need to be digits")
        char_value = int(char)
        if index % 2 == 0:
            data[location] = (char_value, index >> 1)
        else:
            holes[location]((char_value, location))
        location += char_value
