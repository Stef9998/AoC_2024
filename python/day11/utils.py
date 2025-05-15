
def get_numbers(line: str) -> list[int]:
    numbers_str = line.split(' ')
    return [int(number) for number in numbers_str]