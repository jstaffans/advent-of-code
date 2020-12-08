package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

func main() {
	dat, err := ioutil.ReadFile("input08.txt")

	if err != nil {
		panic(err)
	}

	txt := string(dat)
	rawLines := strings.Split(txt, "\n")
	lines := rawLines[:len(rawLines)-1]

	visited := make([]bool, len(lines))
	acc := 0
	i := 0

	for {
		if visited[i] {
			break
		}

		visited[i] = true

		instruction := lines[i]
		parts := strings.Split(instruction, " ")
		command, strOffset := parts[0], parts[1]
		offset, _ := strconv.Atoi(strOffset)

		switch command {
		case "acc":
			acc += offset
			i++
		case "jmp":
			i += offset
		case "nop":
			i++
		}
	}

	fmt.Println(acc)
}
