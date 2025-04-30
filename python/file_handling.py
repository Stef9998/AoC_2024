
def get_file_as_lines() -> list[str]:
    with open('input.txt', 'r') as file:
        data = file.read().splitlines()
    return data
