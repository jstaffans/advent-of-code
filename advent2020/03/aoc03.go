package main

import (
	"fmt"
	"io/ioutil"
	"strings"
)

type treeMap struct {
	h, w int
	treeLocations [][]int
}

func newTreeMap(lines []string) (*treeMap, error) {
	h := len(lines)
	w := len(lines[0])
	rows := make([][]int, h)
	for i, _ := range rows {
		rows[i] = make([]int, w)
		for j, v := range strings.Split(lines[i], "") {
			if v == "#" {
				rows[i][j] = 1
			}
		}
	}

	return &treeMap{
		h,
		w,
		rows,
	}, nil
}

func (m *treeMap) numTreesAt(x, y int) int {
	return m.treeLocations[y][x % m.w]
}

func numTreesForSlope(m *treeMap, right, down int) int {
	numTrees := 0
	x := right
	for y := down; y < m.h; y += down {
		numTrees += m.numTreesAt(x, y)
		x += right
	}
	return numTrees
}

func main() {
	dat, err := ioutil.ReadFile("input03.txt")

	if err != nil {
		panic(err)
	}

	txt := string(dat)
	rawLines := strings.Split(txt, "\n")
	lines := rawLines[:len(rawLines)-1]

	m, err := newTreeMap(lines)

	if err != nil {
		panic(err)
	}

	numTrees := numTreesForSlope(m, 3, 1)

	fmt.Printf("Number of trees slope 3,1: %d\n", numTrees)

	product := numTreesForSlope(m, 1, 1) *
		numTreesForSlope(m, 3, 1) *
		numTreesForSlope(m, 5, 1) *
		numTreesForSlope(m, 7, 1) *
		numTreesForSlope(m, 1, 2)

	fmt.Printf("Product: %d", product)
}
