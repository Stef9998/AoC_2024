from collections.abc import Iterator


def parse_this(lines: list[str]):
    line_splitter = lambda s: tuple(sorted(s.split("-", 1)))
    tuples: Iterator[tuple[str, str]] = map(line_splitter, lines)
    return sorted(tuples)


def fill_uni_dict(tuples) -> dict[str, list[str]]:
    mappings = dict()
    for con in tuples:
        mappings.setdefault(con[0], []).append(con[1])
    return mappings


def fill_uni_dict_set(tuples) -> dict[str, list[str]]:
    mappings = dict()
    for con in tuples:
        mappings.setdefault(con[0], set()).add(con[1])
    return mappings


def fill_bi_dict_set(tuples) -> dict[str, list[str]]:
    mappings = dict()
    for con in tuples:
        mappings.setdefault(con[0], set()).add(con[1])
        mappings.setdefault(con[1], set()).add(con[0])
    return mappings
