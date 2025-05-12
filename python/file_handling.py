def get_file_as_lines() -> list[str]:
    return get_specific_file_as_lines('input.txt')


def get_specific_file_as_lines(text_file):
    with open(text_file, 'r') as file:
        data = file.read().splitlines()
    return data
