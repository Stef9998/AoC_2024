package main

import (
	"aoc2024/util"
	"fmt"
	"regexp"
	"strconv"
	"strings"
)

const day = 3

func main() {
	filename := "input.txt"
	//filename := "sample.txt"
	lines, err := util.ReadLines(day, filename)
	if err != nil {
		panic(fmt.Sprintf("Error reading file: %v", err))
	}

	inBuilder := strings.Builder{}
	for _, line := range lines {
		inBuilder.WriteString(line)
	}
	collectedString := inBuilder.String()

	//res1 := part1(collectedString)
	//fmt.Println("Part 1:", res1)
	res2 := part2(collectedString)
	fmt.Println("Part 2:", res2)
	//res2 = part2rec(collectedString)
	//fmt.Println("Part 2:", res2)
}

func part1(input string) int {
	re := regexp.MustCompile(`mul\((\d+),(\d+)\)`)
	matches := re.FindAllStringSubmatch(input, -1)
	multiplications := 0
	for _, match := range matches {
		//fmt.Printf("Found: %s, x=%s, y=%s\n", match[0], match[1], match[2])
		x, err := strconv.Atoi(match[1])
		if err != nil {
			panic(fmt.Sprintf("Error converting to int: %v", err))
		}
		y, err := strconv.Atoi(match[2])
		if err != nil {
			panic(fmt.Sprintf("Error converting to int: %v", err))
		}
		multiplications += x * y
	}
	return multiplications
}

func part2(input string) int {
	idx := strings.Index(input, "don't()")
	if idx == -1 {
		panic(fmt.Sprintf("No don't() found in string"))
	}
	first := input[:idx]
	rest := input[idx+len("don't()"):]
	multiplications := part1(first)

	re := regexp.MustCompile(`do\(\)(.*?)don't\(\)`)
	matches := re.FindAllStringSubmatch(rest, -1)
	for _, match := range matches {
		multiplications += part1(match[1])
	}
	//handle the last do()
	var err error
	re, err = regexp.Compile(`.*don't\(\).*?do\(\)(.*)`)
	if err == nil {
		match := re.FindStringSubmatch(rest)
		multiplications += part1(match[1])
	}
	return multiplications
}
