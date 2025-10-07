package util

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
)

func ReadLines(day int, filename string) ([]string, error) {
	path := filepath.Join("..", "..", "input", "day"+fmt.Sprint(day), filename)
	file, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var lines []string
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	if err3 := scanner.Err(); err3 != nil {
		return nil, err

	}
	return lines, nil
}

func ReadChars(day int, filename string) ([][]rune, error) {
	lines, err := ReadLines(day, filename)
	if err != nil {
		return nil, err
	}
	mapval := make([][]rune, len(lines))
	for i, line := range lines {
		mapval[i] = make([]rune, len(line))
		for j, char := range line {
			mapval[i][j] = char
		}
	}
	return mapval, err
}
