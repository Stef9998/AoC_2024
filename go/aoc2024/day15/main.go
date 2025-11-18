package main

import (
	ut "aoc2024/day15/util"
	"aoc2024/util"
	"fmt"
)

const day = 15
const printout = false

func main() {
	filename := "input.txt"
	//filename := "sample.txt"
	//filename := "custom.txt"
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
	if res2 != 1543338 {
		panic("wrong result")
	}
}

func part2(mapLines, dirLines []string) int {
	tileMap, isBorderMap, _, startCoordinate, directions := parseInput2(mapLines, dirLines)
	mapData := tileMap.MapData
	robotCoordinate := startCoordinate
	ut.PrintoutMap(mapData)
	for _, direction := range directions {
		step(tileMap, isBorderClosure(isBorderMap), &robotCoordinate, direction)
		if printout {
			fmt.Printf("Direction: %c\n", direction.ToChar())
			ut.PrintoutMap(mapData)
		}
	}
	return getGPS(mapData)
}

func getGPS(mapData [][]ut.Tile) int {
	sum := 0
	for y, line := range mapData {
		for x, tile := range line {
			if tile == ut.LBox {
				sum += 100*y + x
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
		if canMoveBoxes(tileMap, newRobotCoordinate, direction) {
			moveBoxes(tileMap, newRobotCoordinate, direction)
			tileMap.SetTile(*robotCoordinate, ut.Empty)
			*robotCoordinate = newRobotCoordinate
			tileMap.SetTile(*robotCoordinate, ut.Robot)
		}
	default:
		panic("how?")
	}
}

// returns true if was possible to move boxes
func canMoveBoxes(tileMap ut.TileMap, coordinate util.Coordinate, direction util.Direction) bool {
	switch tileMap.GetTile(coordinate) {
	case ut.Empty:
		return true
	case ut.Wall:
		return false
	case ut.LBox:
		rBoxCoord := util.AddCoords(coordinate, util.Coordinate{X: 1, Y: 0})
		newCoord := util.AddCoords(coordinate, direction.Delta())
		newRBoxCoord := util.AddCoords(rBoxCoord, direction.Delta())
		move := false
		if direction == util.Up || direction == util.Down {
			move = canMoveBoxes(tileMap, newCoord, direction) &&
				canMoveBoxes(tileMap, newRBoxCoord, direction)
		} else if direction == util.Right {
			move = canMoveBoxes(tileMap, newRBoxCoord, direction)
		} else {
			panic("This function should never hit LBox when moving to the left")
		}
		return move
	case ut.RBox:
		lBoxCoord := util.AddCoords(coordinate, util.Coordinate{X: -1, Y: 0})
		newCoord := util.AddCoords(coordinate, direction.Delta())
		newLBoxCoord := util.AddCoords(lBoxCoord, direction.Delta())
		move := false
		if direction == util.Up || direction == util.Down {
			move = canMoveBoxes(tileMap, newCoord, direction) &&
				canMoveBoxes(tileMap, newLBoxCoord, direction)
		} else if direction == util.Left {
			move = canMoveBoxes(tileMap, newLBoxCoord, direction)
		} else {
			panic("This function should never hit LBox when moving to the left")
		}
		return move
	default:
		panic("how?")
	}
	return false
}

func moveBoxes(tileMap ut.TileMap, coordinate util.Coordinate, direction util.Direction) {
	switch tileMap.GetTile(coordinate) {
	case ut.Empty:
		return
	case ut.Wall:
		panic("This function should only be called when canMove is true")
	case ut.LBox:
		rBoxCoord := util.AddCoords(coordinate, util.Coordinate{X: 1, Y: 0})
		newCoord := util.AddCoords(coordinate, direction.Delta())
		newRBoxCoord := util.AddCoords(rBoxCoord, direction.Delta())
		if direction == util.Up || direction == util.Down {
			moveBoxes(tileMap, newCoord, direction)
			moveBoxes(tileMap, newRBoxCoord, direction)
		} else if direction == util.Right {
			moveBoxes(tileMap, newRBoxCoord, direction)
		} else {
			panic("This function should never hit LBox when moving to the left")
		}
		tileMap.SetTile(coordinate, ut.Empty)
		tileMap.SetTile(rBoxCoord, ut.Empty)
		tileMap.SetTile(newCoord, ut.LBox)
		tileMap.SetTile(newRBoxCoord, ut.RBox)
		return
	case ut.RBox:
		lBoxCoord := util.AddCoords(coordinate, util.Coordinate{X: -1, Y: 0})
		newCoord := util.AddCoords(coordinate, direction.Delta())
		newLBoxCoord := util.AddCoords(lBoxCoord, direction.Delta())
		if direction == util.Up || direction == util.Down {
			moveBoxes(tileMap, newCoord, direction)
			moveBoxes(tileMap, newLBoxCoord, direction)
		} else if direction == util.Left {
			moveBoxes(tileMap, newLBoxCoord, direction)
		} else {
			panic("This function should never hit LBox when moving to the left")
		}
		tileMap.SetTile(coordinate, ut.Empty)
		tileMap.SetTile(lBoxCoord, ut.Empty)
		tileMap.SetTile(newCoord, ut.RBox)
		tileMap.SetTile(newLBoxCoord, ut.LBox)
		return
	default:
		panic("how?")
	}
	return
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
