from collections import deque
from enum import Enum
from numpy.f2py.auxfuncs import throw_error

from python.file_handling import get_file_as_lines

from part1 import part_one


if __name__ == '__main__':
    part_one_result = part_one(get_file_as_lines())
    print(f"Part one result:\n{part_one_result}")
    # print(part_one(['XMMAAASASMAMSMSMMSASMSMASMXMASSSSMASAMSMASAMXXXXSSSAMX']))
    # print(part_one(['XMMAAASASMAMSMSMMSASMSMASMXMASSSSMASAMSMASAMXXXXSSSAMX',
    #                 'XMMAAASASMAMSMSMMSASMSMASMXMASSSSMASAMSMASAMXXXXSSSAMX']))
    # print(part_one(['XMMS',
    #                 'MMAS',
    #                 'MMAM',
    #                 'XAAS']))
    # part_two_result = part_two(get_lines_as_list_p2())
    # print(f"Part two result:\n{part_two_result}")
