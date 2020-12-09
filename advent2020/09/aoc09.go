package main

import (
	"fmt"
	"io/ioutil"
	"math"
	"strconv"
	"strings"

	"gonum.org/v1/gonum/stat/combin"
)

func findInvalidNumber(numbers []int, lenPreamble int) int {
	combinations := combin.Combinations(lenPreamble, 2)

	for offset := 0; offset < len(numbers); offset++ {
		sums := make([]int, len(combinations))
		for i, c := range combinations {
			for _, term := range c {
				sums[i] += numbers[term+offset]
			}
		}

		found := true
		for _, sum := range sums {
			if numbers[lenPreamble+offset] == sum {
				found = false
				break
			}
		}

		if found {
			return numbers[lenPreamble+offset]
		}
	}

	return -1
}

func findSumWindow(numbers []int, sum int) (int, int) {
	for i := 0; i < len(numbers); i++ {
		for j := 0; j < len(numbers); j++ {
			windowSum := 0
			for k := i; k <= j; k++ {
				windowSum += numbers[k]
			}

			if windowSum == sum {
				return i, j
			}
		}
	}

	return 0, 0
}

func main() {
	dat, err := ioutil.ReadFile("input.txt")

	if err != nil {
		panic(err)
	}

	txt := string(dat)
	lines := strings.Split(txt, "\n")
	numbers := make([]int, len(lines)-1)
	for i, l := range lines[:len(lines)-1] {
		numbers[i], _ = strconv.Atoi(l)
	}

	invalidNumber := findInvalidNumber(numbers, 25)
	windowStart, windowEnd := findSumWindow(numbers, invalidNumber)

	min, max := math.MaxInt64, 0
	for _, n := range numbers[windowStart:windowEnd] {
		if n < min {
			min = n
		}

		if n > max {
			max = n
		}
	}

	fmt.Printf("%d %d\n", min, max)
	fmt.Println(min + max)
}
