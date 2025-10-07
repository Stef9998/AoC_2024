package main

import (
	"aoc2024/util"
	"errors"
	"fmt"
)

const day = 4

func main() {
	filename := "input.txt"
	//filename := "sample.txt"
	raw, err := util.ReadChars(day, filename)
	if err != nil {
		panic(fmt.Sprintf("Error reading file: %v", err))
	}

	data := raw

	res1 := part1(data)
	fmt.Println("Part 1:", res1)
	res2 := part2(data)
	fmt.Println("Part 2:", res2)
}

func part1(input [][]rune) int {
	count := 0
	for _, line := range input {
		count += linexmas(line)
	}
	for i := 0; i < len(input)-3; i++ {
		vcount, err := verticalxmas(input[i : i+4])
		if err != nil {
			panic(err)
		}
		count += vcount
		crosscount, err := crossxmas(input[i : i+4])
		if err != nil {
			panic(err)
		}
		count += crosscount
	}
	return count
}

func linexmas(input []rune) int {
	s := string(input)
	count := 0
	for i := 0; i <= len(s)-4; i++ {
		if s[i:i+4] == "XMAS" || s[i:i+4] == "SAMX" {
			count++
		}
	}
	return count
}

func verticalxmas(lines [][]rune) (int, error) {
	if len(lines) != 4 {
		return 0, errors.New("input must have exactly 4 lines")
	}
	count := 0
	for col := 0; col < len(lines[0]); col++ {
		vertical := []rune{lines[0][col], lines[1][col], lines[2][col], lines[3][col]}
		str := string(vertical)
		if str == "XMAS" || str == "SAMX" {
			count++
		}
	}
	return count, nil
}

func crossxmas(lines [][]rune) (int, error) {
	if len(lines) != 4 {
		return 0, errors.New("input must have exactly 4 lines")
	}
	count := 0
	for col := 0; col < len(lines[0])-3; col++ {
		cross1 := []rune{lines[0][col], lines[1][col+1], lines[2][col+2], lines[3][col+3]}
		cross2 := []rune{lines[0][col+3], lines[1][col+2], lines[2][col+1], lines[3][col]}
		if stringFound(string(cross1)) {
			count++
		}
		if stringFound(string(cross2)) {
			count++
		}
	}
	return count, nil
}

func stringFound(input string) bool {
	if input == "XMAS" || input == "SAMX" {
		return true
	}
	return false
}

func part2(input [][]rune) int {
	count := 0
	for i := 0; i < len(input)-2; i++ {
		split := input[i : i+3]
		found, err := linesxxmas(split)
		if err != nil {
			panic(err)
		}
		count += found
	}
	return count
}

func linesxxmas(input [][]rune) (int, error) {
	found := 0
	if len(input) != 3 {
		return 0, errors.New("input must have exactly 3 lines")
	}
	for i := 0; i < len(input[0])-2; i++ {
		split := [][]rune{input[0][i : i+3], input[1][i : i+3], input[2][i : i+3]}
		isxx, err := isXXmas(split)
		if err != nil {
			return 0, err
		}
		if isxx {
			found++
		}
	}
	return found, nil
}

func isXXmas(input [][]rune) (bool, error) {
	if len(input) != 3 || len(input[0]) != 3 {
		return false, errors.New("input must be 3x3 char")
	}
	if input[1][1] != 'A' {
		return false, nil
	}
	nw := input[0][0]
	no := input[0][2]
	sw := input[2][0]
	so := input[2][2]
	if nw == so || no == sw {
		return false, nil
	}
	notSM := func(char rune) bool {
		if char == 'S' || char == 'M' {
			return false
		}
		return true
	}
	if notSM(nw) || notSM(no) || notSM(sw) || notSM(so) {
		return false, nil
	}
	return true, nil
}
