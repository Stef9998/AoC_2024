import file_handling
from file_handling import input_as_lines
import day25.utils as utils


def main(lines: list[str]) -> int:
    locks, keys = utils.parse(lines)

    return sum(key_might_fit_lock(lock, key) for key in keys for lock in locks)


def key_might_fit_lock(lock: tuple[int, int, int, int, int], key: tuple[int, int, int, int, int]) -> bool:
    """

    :param lock:
    :param key:
    :return:

    >>> key_might_fit_lock((0, 5, 3, 4, 3), (5, 0, 2, 1, 3))
    False
    >>> key_might_fit_lock((0, 5, 3, 4, 3), (3, 0, 2, 0, 1))
    True

    """
    overlap = tuple(l + k for l, k in zip(lock, key))
    return all(map(lambda pin_overlap: 0 <= pin_overlap <= 5, overlap))


if __name__ == '__main__':
    import time

    result = main(input_as_lines('sample.txt'))
    print(f"Sample one result:\n{result}")
    assert result == 3

    start_time = time.time()
    result = main(input_as_lines())
    print(f"Part one result:\n{result}")
    assert result == 3671
    print(f"Time taken: {time.time() - start_time:.3f} seconds")
