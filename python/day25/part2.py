from file_handling import input_as_lines, input_as_lines


def main(lines: list[str]) -> int:



    return 0  # TODO


if __name__ == '__main__':
    import time

    result = main(input_as_lines('sample.txt'))
    print(f"Sample one result:\n{result}")
    assert result == 3

    start_time = time.time()
    result = main(input_as_lines())
    print(f"Part two result:\n{result}")
    # assert result ==
    print(f"Time taken: {time.time() - start_time:.3f} seconds")