package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

type passwordEntry struct {
	letter   string
	min      int
	max      int
	password string
}

func newPasswordEntry(s string) (*passwordEntry, error) {
	parts := strings.Split(s, " ")

	if len(parts) != 3 {
		return nil, fmt.Errorf("Bad input: %s", s)
	}

	bounds := strings.Split(parts[0], "-")
	min, err := strconv.Atoi(bounds[0])
	if err != nil {
		return nil, err
	}
	max, err := strconv.Atoi(bounds[1])
	if err != nil {
		return nil, err
	}
	letter := parts[1][:1]
	password := parts[2][:len(parts[2])]

	return &passwordEntry{
		letter,
		min,
		max,
		password,
	}, nil
}

func (e *passwordEntry) isValid() bool {
	n := 0
	for _, c := range strings.Split(e.password, "") {
		if c == e.letter {
			n = n + 1
		}
	}

	return n >= e.min && n <= e.max
}

func (e *passwordEntry) isValidToboggan() bool {
	letters := strings.Split(e.password, "")
	n := 0
	if letters[e.min-1] == e.letter {
		n = n + 1
	}
	if letters[e.max-1] == e.letter {
		n = n + 1
	}

	return n == 1
}

func main() {
	dat, err := ioutil.ReadFile("input02.txt")

	if err != nil {
		panic(err)
	}

	txt := string(dat)
	rawLines := strings.Split(txt, "\n")
	lines := rawLines[:len(rawLines)-1]
	numValid := 0
	numValidToboggan := 0

	for _, v := range lines {
		e, err := newPasswordEntry(v)

		if err != nil {
			panic(err)
		}

		if e.isValid() {
			numValid = numValid + 1
		}

		if e.isValidToboggan() {
			numValidToboggan = numValidToboggan + 1
		}
	}

	fmt.Printf("Number of valid passwords: %d\n", numValid)
	fmt.Printf("Number of valid passwords (Toboggan): %d", numValidToboggan)
}
