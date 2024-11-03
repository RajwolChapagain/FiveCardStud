package main

import (
    "fmt"
    "os"
)

const HAND_SIZE = 5
var HAND_MAP = [10]string{"High Card", "Pair", "Two Pair", "Three of a Kind", "Straight", "Flush", "Full House", "Four of a Kind", "Straight Flush", "Royal Straight Flush"}

type Hand struct {
    cards []Card 
    handType int
}

func (h *Hand) AddCard(c Card) {
    if len(h.cards) != HAND_SIZE {
        (*h).cards = append((*h).cards, c)
    } else {
        fmt.Println("Error: Hand can't contain more than 5 cards")
        os.Exit(1)
    }
}

func (h Hand) String() string {
    var result string

    for _, card := range h.cards {
        result += card.String()
    }

    return result
}
