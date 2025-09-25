package main

import (
	"aoc2024/util"
	"fmt"
	"slices"
)

const day = 1

func main() {
	filename := "input.txt"
	//filename = "sample.txt"
	lines, err := util.ReadLines(day, filename)
	if err != nil {
		panic(fmt.Sprintf("Error reading file: %v", err))
	}

	arr1 := make([]int, len(lines))
	arr2 := make([]int, len(lines))
	for i, line := range lines {
		arr1[i], arr2[i] = split(line)
	}

	res1 := part1(arr1, arr2)
	fmt.Println("Part 1:", res1)
	res2 := part2(arr1, arr2)
	fmt.Println("Part 2:", res2)
}

func part2(arr1 []int, arr2 []int) int {
	comb := make([]int, len(arr1))
	occurrences := arrayOccurrences(arr2)
	for i, number := range arr1 {
		comb[i] = occurrences(number) * number
	}
	return util.SumInts(comb)
}

func arrayOccurrences(arr []int) func(int) int {
	return func(numb int) int {
		occ := 0
		for _, v := range arr {
			if v == numb {
				occ++
			}
		}
		return occ
	}
}

func part1(arr1 []int, arr2 []int) int {
	slices.Sort(arr1)
	slices.Sort(arr2)
	diff := make([]int, len(arr1))
	for i := range diff {
		if arr2[i] >= arr1[i] {
			diff[i] = arr2[i] - arr1[i]
		} else {
			diff[i] = arr1[i] - arr2[i]
		}
	}
	sum := util.SumInts(diff)
	return sum
}

func split(s string) (int, int) {
	var a, b int
	_, err := fmt.Sscanf(s, "%d %d", &a, &b)
	if err != nil {
		panic(fmt.Sprintf("invalid input format: %q, error: %v", s, err))
	}
	return a, b
}
