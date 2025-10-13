package main

import (
	"aoc2024/util"
	"fmt"
)

const day = 6

func main() {
	filename := "input.txt"
	//filename := "sample.txt"
	raw, err := util.ReadChars(day, filename)
	if err != nil {
		panic(fmt.Sprintf("Error reading file: %v", err))
	}

	mapping := util.NewMapping(raw)
	mapData := mapData(mapping)

	res1 := part1(mapping, mapData)
	fmt.Println("Part 1:", res1)
	res2 := part2(mapping, mapData)
	fmt.Println("Part 2:", res2)
}

func part1(mapping util.Mapping, mapData MapData) int {
	visited := runSim(mapping, mapData)
	return visited.Size()
}

func runSim(mapping util.Mapping, mapData MapData) util.Set[util.Coordinate] {
	visited := util.NewSet[util.Coordinate]()

	for mapping.InBounds(mapData.guardPos) {
		visited.Add(mapData.guardPos)
		nextPos := util.AddCoords(mapData.guardPos, mapData.guardDir.Delta())
		if mapData.obstructions.Contains(nextPos) {
			mapData.guardDir.RotateRight()
			continue
		}
		mapData.guardPos = nextPos
	}
	return visited
}

func part2(mapping util.Mapping, mapData MapData) int {
	loopObstructions := 0
	possibleObstructions := runSim(mapping, mapData)
	possibleObstructions.Remove(mapData.guardPos)
	for obstruction, _ := range possibleObstructions {
		if part2calc(mapping, mapData, obstruction) {
			loopObstructions++
		}
	}
	return loopObstructions
}

func part2calc(mapping util.Mapping, mapData MapData, newObstruction util.Coordinate) bool {
	mapData.obstructions.Add(newObstruction)
	defer mapData.obstructions.Remove(newObstruction)

	visited := util.NewSet[util.Coordinate]()
	visitedTwice := util.NewSet[util.Coordinate]()
	visitedTrice := util.NewSet[util.Coordinate]()
	visitedFourth := util.NewSet[util.Coordinate]()
	loopFound := false
	for mapping.InBounds(mapData.guardPos) {
		if visited.Contains(mapData.guardPos) {
			if visitedTwice.Contains(mapData.guardPos) {
				if visitedTrice.Contains(mapData.guardPos) {
					if visitedFourth.Contains(mapData.guardPos) {
						loopFound = true
						break
					}
					visitedFourth.Add(mapData.guardPos)
				} else {
					visitedTrice.Add(mapData.guardPos)
				}
			} else {
				visitedTwice.Add(mapData.guardPos)
			}
		} else {
			visited.Add(mapData.guardPos)
		}

		nextPos := util.AddCoords(mapData.guardPos, mapData.guardDir.Delta())
		if mapData.obstructions.Contains(nextPos) {
			mapData.guardDir.RotateRight()
			continue
		}
		mapData.guardPos = nextPos
	}
	return loopFound
}

type MapData struct {
	obstructions util.Set[util.Coordinate]
	guardPos     util.Coordinate
	guardDir     util.Direction
}

func mapData(mapping util.Mapping) MapData {
	obstructions := util.NewSet[util.Coordinate]()
	var guardPos util.Coordinate
	var guardDir util.Direction

	for y := 0; y < mapping.Height; y++ {
		for x := 0; x < mapping.Width; x++ {
			char := mapping.Data[y][x]
			coord := util.Coordinate{X: x, Y: y}
			switch char {
			case '#':
				obstructions.Add(coord)
			case '^':
				guardPos = coord
				guardDir = util.Up
			case 'v':
				guardPos = coord
				guardDir = util.Down
			case '<':
				guardPos = coord
				guardDir = util.Left
			case '>':
				guardPos = coord
				guardDir = util.Right
			}
		}
	}

	return MapData{
		obstructions: obstructions,
		guardPos:     guardPos,
		guardDir:     guardDir,
	}
}
