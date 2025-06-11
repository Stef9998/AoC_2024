from collections import deque

from day23.utils import parse_this, fill_bi_dict_set
from file_handling import input_as_lines, input_as_lines


def main(lines: list[str]):
    tuples = parse_this(lines)
    mappings = fill_bi_dict_set(tuples)

    def rec_comb(clique: set[str], to_test: deque[str]):
        if len(to_test) == 0:
            return clique
        next_to_test = to_test.pop()
        connected = mappings.get(next_to_test, set())
        if clique.issubset(connected):
            clique.add(next_to_test)
        return rec_comb(clique, to_test)

    listings = []
    for key, mapping in mappings.items():
        if not key[0] == 't':
            continue
        ret = rec_comb({key}, deque(mapping))
        listings.append(ret)

    biggest_clique = max(listings, key=len)
    return ','.join(sorted(biggest_clique))


if __name__ == '__main__':
    import time

    result = main(input_as_lines('sample.txt'))
    print(f"Sample one result:\n{result}")
    assert result == 'co,de,ka,ta'

    start_time = time.time()
    result = main(input_as_lines())
    print(f"Part one result:\n{result}")
    assert result == "df,kg,la,mp,pb,qh,sk,th,vn,ww,xp,yp,zk"
    print(f"Time taken: {time.time() - start_time:.2f} seconds")
