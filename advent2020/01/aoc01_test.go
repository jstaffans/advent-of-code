package main

import "testing"

func TestFindTerms(t *testing.T) {
	fixture := []int{1, 2, 3, 4, 5, 6, 7, 8, 10}
	combination, _ := findTerms(fixture, 2, 10)
	if combination[0] != 2 || combination[1] != 8 {
		t.Errorf("Want 2 and 8, got %v", combination)
	}


}
