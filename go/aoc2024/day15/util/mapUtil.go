package util

import (
	"aoc2024/util"
	"fmt"
)

type Box struct {
	Left  util.Coordinate
	Right util.Coordinate
}

type TileMap struct {
	MapData [][]Tile
	Height  int
	Width   int
}

func (m *TileMap) GetTile(coordinate util.Coordinate) Tile {
	return m.MapData[coordinate.Y][coordinate.X]
}
func (m *TileMap) SetTile(coordinate util.Coordinate, newTile Tile) {
	m.MapData[coordinate.Y][coordinate.X] = newTile
}

type Tile int

const (
	Wall Tile = iota
	LBox
	RBox
	Robot
	Empty
)

func PrintoutMap(mapping [][]Tile) {
	for _, row := range mapping {
		var rowStr string
		for _, tile := range row {
			var ch rune
			switch tile {
			case Wall:
				ch = '#'
			case LBox:
				ch = '['
			case RBox:
				ch = ']'
			case Robot:
				ch = '@'
			case Empty:
				ch = '.'
			default:
				panic("Enum Broken")
			}
			rowStr += string(ch)
		}
		fmt.Println(rowStr)
	}
}
