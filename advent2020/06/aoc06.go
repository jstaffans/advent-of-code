package main

import (
	"fmt"
	"io/ioutil"
	"strings"
)

func main() {
	dat, err := ioutil.ReadFile("input06.txt")

	if err != nil {
		panic(err)
	}

	txt := string(dat)
	lines := strings.Split(txt, "\n")

	answers := map[string]int{}
	numPersons := 0
	total := 0
	for _, l := range lines {
		if len(l) == 0 {
			for _, respondees := range answers {
				if respondees == numPersons {
					total++
				}
			}

			numPersons = 0
			answers = map[string]int{}
			continue
		}

		questions := strings.Split(l, "")
		for _, q := range questions {
			answers[q]++
		}
		numPersons++
	}

	fmt.Println(total)
}
