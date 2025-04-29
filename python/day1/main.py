from utils import get_input_as_lists


def part_one(list1, list2):
    list.sort(list1)
    list.sort(list2)
    return sum_element_difference(list1, list2)


def sum_element_difference(list1, list2):
    """
    Calculates the sum of absolute differences between corresponding elements of two lists.

    Args:
        list1 (list[int]): The first list of integers.
        list2 (list[int]): The second list of integers.

    Returns:
        int: The sum of absolute differences between corresponding elements of the two lists.

    Example:
        list1 = [1, 2, 3]
        list2 = [4, 5, 6]
        result = sum_element_difference(list1, list2)
        # result = 9 (|1-4| + |2-5| + |3-6|)
    """
    return sum(map(lambda x, y: abs(x - y), list1, list2))


def part_two(list1, list2):
    return calculate_similarity_score(list1, list2)


def calculate_similarity_score(list1, list2):
    return sum(map(lambda x: x * count_occurrences(x, list2), list1))


def count_occurrences(num, lst):
    return lst.count(num)


if __name__ == '__main__':
    list1, list2 = get_input_as_lists()
    print(part_one(list1, list2))
    print(part_two(list1, list2))