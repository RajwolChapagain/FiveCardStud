package main

import (
    "fmt"
    "math/rand"
    "time"
)


func main() {
    hands := [6]Hand{}
    fmt.Println("*** P O K E R   H A N D   A N A L Y Z E R***\n\n")

    deck := CreateDeck()
    PrintDeck(deck)
    DealFromDeck(&hands, &deck)
    PrintHands(hands)
    PrintRemainingDeck(deck)
}

// =============== Non-testing functions ===============

func CreateDeck() []Card {
    deck := []Card{}

    for i := 0; i < 52; i++ {
        deck = append(deck, Card{i % 13, i /13})
    }

    //Shuffle
    rand.Seed(time.Now().UnixNano())
    for i, _ := range deck {
        randomInt := rand.Intn(52)
        
        temp := deck[i]
        deck[i] = deck[randomInt]
        deck[randomInt] = temp
    }

    return deck
}

func PrintDeck(d []Card) {
    fmt.Println("*** USING RANDOMIZED DECK OF CARDS ***\n")

    fmt.Println("*** Shuffled 52 card deck:")
    for i, card := range d {
        fmt.Print(card)

        if ((i+1)%13 == 0) {
            fmt.Println()
        }
    }
    
    fmt.Println()
}

func DealFromDeck(hands *[6]Hand, deck *[]Card) {
    for i := 0; i < HAND_SIZE; i++ {
        for i := range *hands {
            hand := &hands[i]
            hand.AddCard((*deck)[0])
            *deck = (*deck)[1:]
        }
    }
}

func PrintRemainingDeck(deck []Card) {
    fmt.Println("*** Here is what remains in the deck...")
    for _, card := range deck {
        fmt.Print(card)
    }
    fmt.Println()
}

// =============== Common functions ===============

func PrintHands(hands [6]Hand) {
    fmt.Println("*** Here are the six hands...")

    for _, hand := range hands {
        fmt.Println(hand)
    }

    fmt.Println()
}
