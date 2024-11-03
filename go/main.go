package main

import (
    "fmt"
    "math/rand"
    "time"
    "os"
    "bufio"
    "strings"
)


func main() {
    hands := [6]Hand{}
    fmt.Println("*** P O K E R   H A N D   A N A L Y Z E R ***\n\n")

    if len(os.Args) == 2 {
        filePath := os.Args[1]
        PrintFile(filePath)
        DealFromFile(&hands, filePath)
        PrintHands(hands)
    } else {
        deck := CreateDeck()
        PrintDeck(deck)
        DealFromDeck(&hands, &deck)
        PrintHands(hands)
        PrintRemainingDeck(deck)
    }
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

// =============== Testing functions ===============

func PrintFile(path string) {
    fmt.Println("*** USING TEST DECK ***\n")

    fmt.Println("*** File:", path)

    content, err := os.ReadFile(path)

    if err != nil {
        fmt.Println("Error reading file:", err)
        return
    }

    fmt.Println(string(content))
}

func DealFromFile(hands *[6]Hand, path string) {
    file, err := os.Open(path)

    if err != nil {
        fmt.Println("Error opening file:", err)
        return
    }
    defer file.Close()

    i := 0
    // Create a new scanner to read the file line by line
    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        for _, token := range strings.Split(scanner.Text(), ",") {
            (*hands)[i].AddCard(CardFromString(token))
        }

        i += 1
    }

    // Check for errors during scanning
    if err := scanner.Err(); err != nil {
        fmt.Println("Error reading file:", err)
    }
}

// =============== Common functions ===============

func PrintHands(hands [6]Hand) {
    fmt.Println("*** Here are the six hands...")

    for _, hand := range hands {
        fmt.Println(hand)
    }

    fmt.Println()
}
