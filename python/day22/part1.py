from day22.utils import secret_number_after_iterations
from file_handling import input_as_lines, input_as_lines


def main(lines: list[str]) -> int:
    return sum(secret_number_after_iterations(2000, int(secret_number)) for secret_number in lines)

if __name__ == '__main__':

    import time

    # result = main(input_as_lines('sample.txt'))
    # print(f"Sample result:\n{result}")
    # assert result ==

    start_time = time.time()
    result = main(input_as_lines())
    print(f"Part one result:\n{result}")
    assert result == 17965282217
    print(f"Time taken: {time.time() - start_time:.2f} seconds")
