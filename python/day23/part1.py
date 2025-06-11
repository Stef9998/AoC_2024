import itertools

from day23.utils import parse_this, fill_uni_dict
from file_handling import input_as_lines, input_as_lines


def main(lines: list[str]):
    tuples = parse_this(lines)
    mappings = fill_uni_dict(tuples)

    three_count = 0
    for key, mapping in mappings.items():
        key_t = key[0] == 't'
        for sec, thr in itertools.combinations(mapping, 2):
            if not (key_t or sec[0] == 't' or thr[0] == 't'):
                continue
            if thr in mappings.get(sec, []):
                three_count += 1

    return three_count


if __name__ == '__main__':
    import time

    result = main(input_as_lines('sample.txt'))
    print(f"Sample one result:\n{result}")
    assert result == 7

    start_time = time.time()
    result = main(input_as_lines())
    print(f"Part one result:\n{result}")
    assert result < 4510
    assert result != 1365
    print(f"Time taken: {time.time() - start_time:.2f} seconds")
