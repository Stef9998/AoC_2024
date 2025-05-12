def get_information(lines: list[str]):
    line = lines[0]
    data = []
    empty = []
    for index,char in enumerate(line):
        if not char.isnumeric():
            raise ValueError("Invalid data provided to the function.\n"
                             "The chars need to be digits")
        if index % 2 == 0:
            data.append((int(char),index>>1))
        else:
            # empty.append(((index-1)>>1,int(char)))
            empty.append(int(char))
    return data, empty
