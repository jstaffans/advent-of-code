package main

import (
	"fmt"
	"io/ioutil"
	"sort"
	"strconv"
	"strings"
)

func getDiffs(ratings []int) (int, int) {
	prevRating := 0
	d1, d2 := 0, 0

	for _, rating := range ratings {
		if rating - prevRating == 1 {
			d1++
		}

		if rating - prevRating == 3 {
			d2++
		}

		prevRating = rating
	}

	// device built-in adapter always three jolts above
	d2++
	return d1, d2
}

func getNumPathsTo(ratings []int, maxRating int) int {
	paths := map[int]int{}
	paths[0] = 1

	for _, rating := range ratings {
		for diff := 1; diff < 4; diff++ {
			nextAdapter := rating + diff
			nextRatingIndex := sort.SearchInts(ratings, nextAdapter)
			if nextRatingIndex < len(ratings) {
				paths[nextAdapter] += paths[rating]
			}
		}
	}

	return paths[maxRating]
}

func main() {
	dat, err := ioutil.ReadFile("input.txt")

	if err != nil {
		panic(err)
	}

	txt := string(dat)
	lines := strings.Split(txt, "\n")
	ratings := make([]int, len(lines)-1)
	for i, l := range lines[:len(lines)-1] {
		ratings[i], _ = strconv.Atoi(l)
	}

	ratings = append(ratings, 0)
	sort.Ints(ratings)

	fmt.Println(ratings)

	d1, d2 := getDiffs(ratings)
	fmt.Printf("%d, %d\n", d1, d2)

	numPaths := getNumPathsTo(ratings, ratings[len(ratings)-1])
	fmt.Println(numPaths)
}
