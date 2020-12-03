package main

import (
	"testing"
)

func TestNewTreeMap(t *testing.T) {
	fixture := []string{
		"..#",
		".#.",
	}

	treeMap, _ := newTreeMap(fixture)

	if treeMap.h != 2 || treeMap.w != 3 {
		t.Errorf("Expected h=2, w=3, got: h=%d, w=%d", treeMap.h, treeMap.w)
	}

	if treeMap.treeLocations[0][2] != 1 {
		t.Errorf("First row incorrect, expected [0 0 1], got: %v", treeMap.treeLocations[0])
	}
}

func TestNumTreesAt(t *testing.T) {
	fixture := []string{
		"..#",
		".#.",
		"#..",
	}

	treeMap, _ := newTreeMap(fixture)

	treeFixture := [][]int{
		{0, 0, 0},
		{1, 0, 0},
		{2, 0, 1},
		{0, 1, 0},
		{1, 1, 1},
		{2, 1, 0},
		{3, 0, 0},
		{4, 1, 1},
	}

	for _, f := range treeFixture {
		x := f[0]
		y := f[1]
		expected := f[2]
		if treeMap.numTreesAt(x, y) != expected {
			t.Errorf("%d trees expected at %d, %d", expected, x, y)
		}
	}

}
