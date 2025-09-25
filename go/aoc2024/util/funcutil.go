package util

// SumInts returns the sum of a slice of ints.
func SumInts(arr []int) int {
	total := 0
	for _, v := range arr {
		total += v
	}
	return total
}
