def get_file_as_lines() -> list[str]:
    return get_specific_file_as_lines('input.txt')


def get_specific_file_as_lines(text_file):
    with open(text_file, 'r') as file:
        data = file.read().splitlines()
    return data


def input_as_lines(filename="input.txt") -> list[str]:
    return get_input_file(filename, 2).splitlines()


import inspect
from pathlib import Path

def get_input_file(filename="input.txt", stack_frame = 1) -> str:
    """
    Returns the contents of the specified input file (default: 'input.txt'),
    based on the calling script's day directory (e.g., 'day1', 'day2').
    """
    # Get the path of the calling script (e.g., python/day1/main.py)
    caller_frame = inspect.stack()[stack_frame]
    caller_path = Path(caller_frame.filename).resolve()

    # Get the name of the day folder (e.g., 'day1')
    day_folder = caller_path.parent.name

    # Construct the input file path
    input_path = Path(__file__).resolve().parent.parent / "input" / day_folder / filename

    # Read and return the content
    return input_path.read_text()