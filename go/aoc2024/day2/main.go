package main

import (
	"aoc2024/util"
	"fmt"
	"strconv"
	"strings"
)

const day = 2

func main() {
	filename := "input.txt"
	//filename := "sample.txt"
	lines, err := util.ReadLines(day, filename)
	if err != nil {
		panic(fmt.Sprintf("Error reading file: %v", err))
	}

	linesParsed := make([][]int, len(lines))
	for i, line := range lines {
		linesParsed[i] = splitInts(line)
	}

	res1 := part1(linesParsed)
	fmt.Println("Part 1:", res1)
	res2 := part2(linesParsed)
	fmt.Println("Part 2:", res2)
}

func part1(lines [][]int) int {
	return util.Count(lines, reportIsSafe)
}

func part2(lines [][]int) int {
	saveReports := 0
	for _, report := range lines {
		if reportIsSafe(report) {
			saveReports++
		} else {
			for i := 0; i < len(report); i++ {
				modified := append(append([]int{}, report[:i]...), report[i+1:]...)
				if reportIsSafe(modified) {
					saveReports++
					break
				}
			}
		}
	}
	return saveReports
}

func reportIsSafe(report []int) bool {
	return reportIsIncreasing(report) || reportIsDecreasing(report)
}

func reportIsIncreasing(report []int) bool {
	for i := 1; i < len(report); i++ {
		diff := report[i] - report[i-1]
		if diff < 1 {
			return false
		}
		if diff > 3 {
			return false
		}
	}
	return true
}
func reportIsDecreasing(report []int) bool {
	for i := 1; i < len(report); i++ {
		diff := report[i] - report[i-1]
		if diff > -1 {
			return false
		}
		if diff < -3 {
			return false
		}
	}
	return true
}

func splitInts(s string) []int {
	fields := strings.Fields(s)
	result := make([]int, len(fields))
	for i, field := range fields {
		n, err := strconv.Atoi(field)
		if err != nil {
			panic(fmt.Sprintf("invalid input format: %q, error: %v", s, err))
		}
		result[i] = n
	}
	return result
}
