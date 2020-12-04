package main

import (
	"fmt"
	"io/ioutil"
	"regexp"
	"strconv"
	"strings"
)

func checkYear(passport map[string]string, field string, min, max int) bool {
    year, err := strconv.Atoi(passport[field])
	if err != nil {
		return false
	}
	if year < min || year > max {
		return false
	}
	return true
}

func isPassportValid(passport map[string]string) bool {
	required := []string{
		"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid",
	}

	for _, r := range required {
		_, present := passport[r]
		if !present {
			return false
		}
	}

	if checkYear(passport, "byr", 1920, 2002) == false {
		return false
	}

	if checkYear(passport, "iyr", 2010, 2020) == false {
		return false
	}

	if checkYear(passport, "eyr", 2020, 2030) == false {
		return false
	}

	match, _ := regexp.MatchString("^\\d+(cm|in)$", passport["hgt"])
	if !match {
		return false
	}

	hgt := passport["hgt"]
	unt := hgt[len(hgt)-2:]
	val, err := strconv.Atoi(hgt[:len(hgt)-2])
	if err != nil {
		return false
	}
	if unt == "cm" && (val < 150 || val > 193) {
		return false
	}
	if unt == "in" && (val < 59 || val > 76) {
		return false
	}

	match, _ = regexp.MatchString("^#[0-9a-f]{6}$", passport["hcl"])
	if !match {
		return false
	}

	match, _ = regexp.MatchString("^(amb|blu|brn|gry|grn|hzl|oth)$", passport["ecl"])
	if !match {
		return false
	}

	match, _ = regexp.MatchString("^[0-9]{9}$", passport["pid"])
	if !match {
		return false
	}

	return true
}

func splitData(s, sep string) (string, string) {
	temp := strings.Split(s, sep)
	return temp[0], temp[1]
}

func main() {
	dat, err := ioutil.ReadFile("input04.txt")

	if err != nil {
		panic(err)
	}

	txt := string(dat)
	rawLines := strings.Split(txt, "\n")
	lines := rawLines[:len(rawLines)-1]
	passports := make([]map[string]string, 1)
	passports[0] = map[string]string{}
	i := 0

	for _, l := range lines {
		if len(l) == 0 {
			passports = append(passports, map[string]string{})
			i++
			continue
		}

		parts := strings.Split(l, " ")
		for _, p := range parts {
			k, v := splitData(p, ":")
			passports[i][k] = v
		}
	}

	numValid := 0
	for _, p := range passports {
		if isPassportValid(p) {
			numValid++
		}
	}

	fmt.Printf("Total number of passports: %d\n", len(passports))
	fmt.Printf("Number of valid passports: %d\n", numValid)
}
