from file_handling import get_specific_file_as_lines, get_file_as_lines


def main(lines: list[str]) -> int:

    return 0


if __name__ == '__main__':
    import time

    result = main(get_specific_file_as_lines('sample_input.txt'))
    print(f"Sample one result:\n{result}")
    assert result == 3

    start_time = time.time()
    result = main(get_file_as_lines())
    print(f"Part one result:\n{result}")
    # assert result ==
    print(f"Time taken: {time.time() - start_time:.3f} seconds")
