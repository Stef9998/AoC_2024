package main

import (
	"aoc2024/util"
	"fmt"
	"strconv"
	"strings"
	"sync"
)

const day = 7

var wg = sync.WaitGroup{}

func main() {
	filename := "input.txt"
	//filename := "sample.txt"
	raw, err := util.ReadLines(day, filename)
	if err != nil {
		panic(fmt.Sprintf("Error reading file: %v", err))
	}

	equations := make([]equation, len(raw))
	for i, line := range raw {
		equations[i] = parseInput(line)
	}

	res1 := part1(equations)
	fmt.Println("Part 1:", res1)
	if res1 != 850435817339 {
		panic("wrong result")
	}
	res2 := part2(equations)
	fmt.Println("Part 2:", res2)
	if res2 != 104824810233437 {
		panic("wrong result")
	}
}

func parseInput(line string) equation {
	splitLine := strings.Split(line, ": ")
	if len(splitLine) != 2 {
		panic("error parsing file")
	}
	result, err := strconv.Atoi(splitLine[0])
	if err != nil {
		panic("error parsing file")
	}
	operands, err := util.SeparatedNumbers(splitLine[1], " ")
	if err != nil {
		panic("error parsing file")
	}
	return equation{result, operands}
}

func part1(equations []equation) int {
	sum := 0
	for _, equ := range equations {
		sum += part1calc(equ)
	}
	return sum
}

func part1calc(equ equation) int {
	if len(equ.operands) == 1 {
		if equ.result == equ.operands[0] {
			return equ.result
		} else {
			return 0
		}
	}
	index := 1
	currentCalc := equ.operands[0]
	return p1rec(equ, index, currentCalc)
}

func p1rec(equ equation, index int, current int) int {
	if index == len(equ.operands) {
		if equ.result == current {
			return equ.result
		} else {
			return 0
		}
	}

	res := p1rec(equ, index+1, current+equ.operands[index])
	if res != 0 {
		return res
	}
	res = p1rec(equ, index+1, current*equ.operands[index])
	if res != 0 {
		return res
	}
	return 0
}

func part2(equations []equation) int {
	sum := 0
	for _, equ := range equations {
		sum += part2calc(equ)
	}
	return sum
}

func part2calc(equ equation) int {
	if len(equ.operands) == 1 {
		if equ.result == equ.operands[0] {
			return equ.result
		} else {
			return 0
		}
	}
	index := 1
	currentCalc := equ.operands[0]
	return p2rec(equ, index, currentCalc)
}

func p2rec(equ equation, index int, current int) int {
	if index == len(equ.operands) {
		if equ.result == current {
			return equ.result
		} else {
			return 0
		}
	}

	res := p2rec(equ, index+1, current+equ.operands[index])
	if res != 0 {
		return res
	}
	res = p2rec(equ, index+1, current*equ.operands[index])
	if res != 0 {
		return res
	}
	str := strconv.Itoa(current) + strconv.Itoa(equ.operands[index])
	newNumb, err := strconv.Atoi(str)
	if err != nil {
		panic("conv of string to int should be possible, as were int before")
	}
	res = p2rec(equ, index+1, newNumb)
	if res != 0 {
		return res
	}
	return 0
}

type equation struct {
	result   int
	operands []int
}
