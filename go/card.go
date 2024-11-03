package main

import (
    "strings"
    "fmt"
    "os"
)

var VALUE_MAP = [13]string{"A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"}
var SUIT_MAP = [4]string{"D", "C", "H", "S"}

type Card struct {
    value, suit int 
}

func CardFromString(cardString string) Card {
    cardString = strings.TrimSpace(cardString) 
   
    valueString := cardString[:len(cardString) - 1]
    suitString := cardString[len(cardString)-1:]

    valueIndex := -1
    suitIndex := -1

    for i, value := range VALUE_MAP {
        if value == valueString {
            valueIndex = i
            break
        }
    }

    for i, suit := range SUIT_MAP {
        if suit == suitString {
            suitIndex = i
            break
        }
    }

    if valueIndex == -1 {
        fmt.Println("Error: Can't create card with value", valueString)
        os.Exit(1)
    }

    if suitIndex == -1 {
        fmt.Println("Error: Can't create card with suit", suitString)
        os.Exit(1)
    }
    
    return Card{valueIndex, suitIndex} 
}

func GetRawString(c Card) string {
    return VALUE_MAP[c.value] + SUIT_MAP[c.suit]
}

//=============== Sort interface methods ===============

type ByValue []Card

func (v ByValue) Len() int {
    return len(v)
}

func (v ByValue) Less(i, j int) bool {
    return v[i].value < v[j].value
}

func (v ByValue) Swap(i, j int) {
    v[i], v[j] = v[j], v[i]
}

//=============== Stringer interface method ===============
func (c Card) String() string {
    rawString := VALUE_MAP[c.value] + SUIT_MAP[c.suit]
    return fmt.Sprintf("%-4s", rawString)
}

