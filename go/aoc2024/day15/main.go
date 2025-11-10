package main

import (
	ut "aoc2024/day15/util"
	"aoc2024/util"
	"fmt"
)

const day = 15

func main() {
	//filename := "input.txt"
	filename := "sample.txt"
	lines, err := util.ReadLines(day, filename)
	if err != nil {
		panic(fmt.Sprintf("Error reading file: %v", err))
	}
	mapLines, dirLines, err := util.SplitAtEmptyLine(lines)
	if err != nil {
		panic(fmt.Sprintf("Error splitting at empty line: %v", err))
	}
	res2 := part2(mapLines, dirLines)
	fmt.Println("Part 2:", res2)
	//if res2 !=  {
	//	panic("wrong result")
	//}
}

func part2(mapLines, dirLines []string) int {
	tileMap, isBorderMap, _, startCoordinate, directions := parseInput2(mapLines, dirLines)
	mapData := tileMap.MapData
	robotCoordinate := startCoordinate
	ut.PrintoutMap(mapData)
	for _, direction := range directions {
		step(tileMap, isBorderClosure(isBorderMap), &robotCoordinate, direction)
		ut.PrintoutMap(mapData)
	}
	return getGPS(mapData)
}

func getGPS(mapData [][]ut.Tile) int {
	sum := 0
	for y, line := range mapData {
		for x, tile := range line {
			if tile == ut.LBox {
				sum += 100 * x * y
			}
		}
	}
	return sum
}

func step(tileMap ut.TileMap, isBorder func(coordinate util.Coordinate) bool, robotCoordinate *util.Coordinate, direction util.Direction) {
	newRobotCoordinate := util.AddCoords(*robotCoordinate, direction.Delta())
	switch tileMap.GetTile(newRobotCoordinate) {
	case ut.Empty:
		tileMap.SetTile(*robotCoordinate, ut.Empty)
		*robotCoordinate = newRobotCoordinate
		tileMap.SetTile(*robotCoordinate, ut.Robot)
	case ut.Wall:
		//do nothing
	case ut.LBox, ut.RBox:
		if moveBoxes(tileMap, newRobotCoordinate) {
			tileMap.SetTile(*robotCoordinate, ut.Empty)
			*robotCoordinate = newRobotCoordinate
			tileMap.SetTile(*robotCoordinate, ut.Robot)
		}
	default:
		panic("how?")
	}
}

// returns true if was possible to move boxes
func moveBoxes(tileMap ut.TileMap, newRobotCoordinate util.Coordinate) bool {
	//TODO
	return false
}

func isBorderClosure(isBorderMap [][]bool) func(util.Coordinate) bool {
	return func(coordinate util.Coordinate) bool {
		return isBorderMap[coordinate.Y][coordinate.X]
	}
}

func parseInput2(mapLines, dirLines []string) (ut.TileMap, [][]bool, []ut.Box, util.Coordinate, []util.Direction) {
	mapChars, err := util.LinesToChars(mapLines)
	if err != nil {
		panic(fmt.Sprintln("Not able to create map"))
	}
	mapping := util.NewMapping(mapChars)
	mapData, isBorderMap, boxes, startCoordinate, err := getMapData(mapping)
	if err != nil {
		panic(fmt.Sprintln("wrong character in map to parse"))
	}
	directions := getDirs(dirLines)

	return mapData, isBorderMap, boxes, startCoordinate, directions
}

func getDirs(lines []string) []util.Direction {
	directions := make([]util.Direction, 0, len(lines)*len(lines[0]))
	for _, line := range lines {
		for _, char := range line {
			directions = append(directions, util.CharToDir(char))
		}
	}
	return directions
}

func getMapData(mapping util.Mapping) (ut.TileMap, [][]bool, []ut.Box, util.Coordinate, error) {
	newMappingData := make([][]ut.Tile, len(mapping.Data))
	isBorderMap := make([][]bool, len(mapping.Data))
	boxes := make([]ut.Box, 0, 128)
	var startCoordinate util.Coordinate
	for y := 0; y < mapping.Height; y++ {
		newMappingData[y] = make([]ut.Tile, mapping.Width*2)
		isBorderMap[y] = make([]bool, mapping.Width*2)
		for x := 0; x < mapping.Width; x++ {
			char := mapping.Data[y][x]
			switch char {
			case '#':
				newMappingData[y][2*x] = ut.Wall
				newMappingData[y][2*x+1] = ut.Wall
				isBorderMap[y][2*x] = true
				isBorderMap[y][2*x+1] = true
			case 'O':
				newMappingData[y][2*x] = ut.LBox
				newMappingData[y][2*x+1] = ut.RBox
				boxes = append(boxes, ut.Box{Left: util.Coordinate{X: 2 * x, Y: y}, Right: util.Coordinate{X: 2*x + 1, Y: y}})
			case '@':
				newMappingData[y][2*x] = ut.Robot
				newMappingData[y][2*x+1] = ut.Empty
				startCoordinate = util.Coordinate{X: 2 * x, Y: y}
			case '.':
				newMappingData[y][2*x] = ut.Empty
				newMappingData[y][2*x+1] = ut.Empty
			default:
				return ut.TileMap{}, nil, nil, util.Coordinate{}, fmt.Errorf("wrong character '%c' in map", char)
			}
		}
	}
	return ut.TileMap{MapData: newMappingData, Height: len(newMappingData), Width: len(newMappingData[0])},
		isBorderMap, boxes, startCoordinate, nil
}
