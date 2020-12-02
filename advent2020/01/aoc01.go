package main

import (
	"errors"
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"

	"gonum.org/v1/gonum/stat/combin"
)

func findTerms(numbers []int, numTerms int, sum int) ([]int, error) {
	list := combin.Combinations(len(numbers), numTerms)
	for _, combination := range list {
		cSum := 0
		for _, v := range combination {
			cSum += numbers[v]
		}
		if cSum == sum {
			result := make([]int, len(combination))
			for i, v := range combination {
				result[i] = numbers[v]
			}
			return result, nil
		}
	}

	return nil, errors.New("No combination found")
}

func main() {
	dat, err := ioutil.ReadFile("input01.txt")

	if err != nil {
		panic(err)
	}

	txt := string(dat)
	lines := strings.Split(txt, "\n")
	strValues := lines[:len(lines)-1]
	values := make([]int, len(strValues))

	for i, v := range strValues {
		values[i], err = strconv.Atoi(v)
		if err != nil {
			panic(err)
		}
	}

	pair, err := findTerms(values, 2, 2020)
	fmt.Printf("Pair: %v\n", pair)
	fmt.Printf("Product: %d\n", pair[0]*pair[1])

	triplet, err := findTerms(values, 3, 2020)

	fmt.Printf("Triplet: %v\n", triplet)
	fmt.Printf("Product: %d\n", triplet[0]*triplet[1]*triplet[2])
}
