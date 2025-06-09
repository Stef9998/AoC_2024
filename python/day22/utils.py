def secret_number_after_iterations(count: int, secret_number: int) -> int:
    """

    :param count:
    :param secret_number:
    :return:

    """
    for _ in range(count):
        secret_number = next_secret_number(secret_number)
    return secret_number


def next_secret_number(secret_number: int) -> int:
    """

    :param secret_number:
    :return:

    >>> next_secret_number(123)
    15887950
    >>> next_secret_number(15887950)
    16495136
    >>> next_secret_number(7753432)
    5908254
    """
    num_1 = secret_number << 6  # * 64
    secret_number = prune(mix(secret_number, num_1))
    num_2 = secret_number >> 5  # / 32
    secret_number = prune(mix(secret_number, num_2))
    num_3 = secret_number << 11  # * 2024
    secret_number = prune(mix(secret_number, num_3))
    return secret_number


def mix(secret_number: int, value: int) -> int:
    """

    :param secret_number:
    :param value:
    :return:

    >>> mix(42, 15)
    37
    """
    return secret_number ^ value


def prune(secret_number: int) -> int:
    """

    :param secret_number:
    :return:

    >>> prune(100000000)
    16113920
    """
    return secret_number % 16777216
