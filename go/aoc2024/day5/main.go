package main

import (
	"aoc2024/util"
	"fmt"
	"slices"
)

const day = 5

func main() {
	filename := "input.txt"
	//filename := "sample.txt"
	raw, err := util.ReadLines(day, filename)
	if err != nil {
		panic(fmt.Sprintf("Error reading file: %v", err))
	}

	rulesRaw, orderingsRaw, err := util.SplitAtEmptyLine(raw)
	if err != nil {
		panic(err)
	}
	rules := make([][2]int, len(rulesRaw))
	for i, value := range rulesRaw {
		ints, err := util.SeparatedNumbers(value, "|")
		copy(rules[i][:], ints)
		if err != nil {
			panic(err)
		}
	}
	orderings := make([][]int, len(orderingsRaw))
	for i, value := range orderingsRaw {
		orderings[i], err = util.SeparatedNumbers(value, ",")
		if err != nil {
			panic(err)
		}
	}

	rulesMap := make(map[int][]int)
	for _, rule := range rules {
		rulesMap[rule[0]] = append(rulesMap[rule[0]], rule[1])
	}

	res1 := part1(rulesMap, orderings)
	fmt.Println("Part 1:", res1)
	//res2 := part2(rulesMap, orderings)
	//fmt.Println("Part 2:", res2)
}

func part1(rules map[int][]int, orderings [][]int) int {
	sum := 0
	for _, ordering := range orderings {
		sum += part1calc(rules, ordering)
	}
	return sum
}

func part1calc(rules map[int][]int, ordering []int) int {
	if isOrderingValid(rules, ordering) {
		return ordering[(len(ordering)-1)/2]
	}
	return 0
}

func isOrderingValid(rules map[int][]int, ordering []int) bool {
	for i := len(ordering) - 1; i > 0; i-- {
		valrules := rules[ordering[i]]
		for j := 0; j < i; j++ {
			compareval := ordering[j]
			if slices.Contains(valrules, compareval) {
				return false
			}
		}
	}
	return true
}
