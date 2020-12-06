package main

import (
	"fmt"
	"io/ioutil"
	"sort"
	"strings"
)

func partition(half string, min, max int) (int, int) {
	rows := max - min
	switch half {
	case "F", "L":
		return min, min + (rows / 2)
	case "R", "B":
		return min + (rows / 2), max
	}

	return min, max
}

func main() {
	dat, err := ioutil.ReadFile("input05.txt")

	if err != nil {
		panic(err)
	}

	txt := string(dat)
	rawLines := strings.Split(txt, "\n")
	lines := rawLines[:len(rawLines)-1]

	maxId := 0
	ids := make([]int, 0)
	for _, l := range lines {
		rows := l[:7]
		seats := l[7:]
		min, max := 0, 128
		for _, c := range strings.Split(rows, "") {
			min, max = partition(c, min, max)
		}
		id := min * 8
		min, max = 0, 8
		for _, s := range strings.Split(seats, "") {
			min, max = partition(s, min, max)
		}
		id += min
		if id > maxId {
			maxId = id
		}
		ids = append(ids, id)
	}

	sort.Ints(ids)
	minId := ids[0]
	for i, id := range ids {
		if id-minId != i {
			fmt.Println(id-1)
		}
	}
}
