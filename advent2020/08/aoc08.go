package main

import (
	"fmt"
	"io/ioutil"
	"regexp"
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

	acc := 0
	i := 0

	lineToSwitch := 0
	r := regexp.MustCompile("^(nop|jmp).+$")

	updatedLines := make([]string, len(lines))

	switchInstruction := func(s string) string {
		parts := strings.Split(s, " ")
		if parts[0] == "nop" {
			return "jmp " + parts[1]
		} else {
			return "nop " + parts[1]
		}
	}

	for i < len(lines) {
		for !r.MatchString(lines[lineToSwitch]) {
			lineToSwitch++
		}

		copy(updatedLines, lines)
		updatedLines[lineToSwitch] = r.ReplaceAllStringFunc(lines[lineToSwitch], switchInstruction)

		visited := make([]bool, len(lines))
		acc = 0
		i = 0

		for {
			if i >= len(lines) || visited[i] {
				lineToSwitch++
				break
			}

			visited[i] = true

			instruction := updatedLines[i]
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
	}

	fmt.Println(acc)
}
