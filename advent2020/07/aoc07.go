package main

import (
	"fmt"
	"io/ioutil"
	"regexp"
	"strconv"
	"strings"
)

type Graph struct {
	nodes map[string]*Node
}

type Node struct {
	id       string
	children map[string]int
}

func newGraph(lines []string) *Graph {
	graph := &Graph{
		nodes: map[string]*Node{},
	}

	r, _ := regexp.Compile("^(\\w+ \\w+) bags contain (.+)$")
	for _, l := range lines {
		parts := r.FindStringSubmatch(l)
		parent := parts[1]
		children := strings.Split(parts[2], ", ")
		amounts := make([]int, len(children))

		if children[0] == "no other bags." {
			continue
		}

		for i, child := range children {
			id := strings.Split(child, " ")
			children[i] = fmt.Sprintf("%s %s", id[1], id[2])
			amounts[i], _ = strconv.Atoi(id[0])
		}

		_, parentExists := graph.nodes[parent]
		if !parentExists {
			graph.nodes[parent] = &Node{
				id:       parent,
				children: map[string]int{},
			}
		}

		for i, child := range children {
			_, childExists := graph.nodes[child]
			if !childExists {
				graph.nodes[child] = &Node{
					id:       child,
					children: map[string]int{},
				}
			}
			graph.nodes[parent].children[child] = amounts[i]
		}
	}

	return graph
}

func (graph *Graph) visitParentsOf(id string) map[string]bool {
	visited := map[string]bool{}
	for _, node := range graph.nodes {
		_, containsShinyGold := node.children["shiny gold"]
		if containsShinyGold {
			visited[node.id] = true
		}
	}

	numVisited := len(visited)
	numNewVisited := numVisited

	for numNewVisited > 0 {
		for visitedNode := range visited {
			for _, node := range graph.nodes {
				_, containsVisited := node.children[visitedNode]
				if containsVisited && !visited[node.id] {
					visited[node.id] = true
				}
			}
		}

		numNewVisited = len(visited) - numVisited
		numVisited = len(visited)
	}

	return visited
}

func (graph *Graph) countBags(id string) int {
	node := graph.nodes[id]
	childTotal := 0
	for child, amount := range node.children {
		childTotal += amount + amount*graph.countBags(child)
	}
	return childTotal
}

func main() {
	dat, err := ioutil.ReadFile("input07.txt")

	if err != nil {
		panic(err)
	}

	txt := string(dat)
	rawLines := strings.Split(txt, "\n")
	lines := rawLines[:len(rawLines)-1]

	graph := newGraph(lines)
	visited := graph.visitParentsOf("shiny gold")
	total := graph.countBags("shiny gold")

	fmt.Println(len(visited))
	fmt.Println(total)
}
