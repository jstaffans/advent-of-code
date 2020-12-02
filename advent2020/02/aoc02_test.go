package main

import "testing"

func TestNewPasswordEntry(t *testing.T) {
	fixture := "3-11 v: tgvvttgvv"
	fixtureEntry := &passwordEntry{
		"v",
		3,
		11,
		"tgvvttgvv",
	}

	entry, _ := newPasswordEntry(fixture)

	if entry.min != 3 || entry.max != 11 || entry.letter != "v" || entry.password != "tgvvttgvv" {
		t.Errorf("Expected: %v, got: %v", fixtureEntry, entry)
	}
}

func TestInvalid(t *testing.T) {
	fixture := "3-11 v: tgvvttg"
	entry, _ := newPasswordEntry(fixture)

	if entry.isValid() {
		t.Errorf("Should be invalid: %v", fixture)
	}
}

func TestValid(t *testing.T) {
	fixture := "3-11 v: tgvvttgvv"
	entry, _ := newPasswordEntry(fixture)

	if !entry.isValid() {
		t.Errorf("Should be valid: %v", fixture)
	}
}

func TestInvalidToboggan(t *testing.T) {
	fixture := "3-6 v: tgvvtvg"
	entry, _ := newPasswordEntry(fixture)

	if entry.isValidToboggan() {
		t.Errorf("Should be invalid: %v", fixture)
	}
}

func TestValidToboggan(t *testing.T) {
	fixture := "3-6 v: tgvvttgvv"
	entry, _ := newPasswordEntry(fixture)

	if !entry.isValid() {
		t.Errorf("Should be valid: %v", fixture)
	}
}
