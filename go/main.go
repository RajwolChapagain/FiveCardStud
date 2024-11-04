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

        if HasDuplicate(hands) {
            return
        }

        PrintHands(hands)
        AssignTypes(&hands)
        PrintRankedHands(hands)
    } else {
        deck := CreateDeck()
        PrintDeck(deck)
        DealFromDeck(&hands, &deck)
        PrintHands(hands)
        PrintRemainingDeck(deck)
        AssignTypes(&hands)
        PrintRankedHands(hands)
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
    fmt.Println("\n")
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

func HasDuplicate(hands [6]Hand) bool {
    hashes := []int{}

    for _, hand := range hands {
        for _, card := range hand.cards {
            hash := card.value * 10 + card.suit

            for _, elem := range hashes {
                if elem == hash {
                    fmt.Println("*** ERROR - DUPLICATED CARD FOUND IN DECK ***\n")

                    fmt.Println("*** Duplicate: " + card.GetRawString())

                    return true
                }
            }
                   
            hashes = append(hashes, hash)
        }
    }

    return false
}

// =============== Common functions ===============

func PrintHands(hands [6]Hand) {
    fmt.Println("*** Here are the six hands...")

    for _, hand := range hands {
        fmt.Println(hand)
    }

    fmt.Println()
}

func AssignTypes(hands *[6]Hand) {
    for i, _ := range hands {
        AssignType(&hands[i])
    }
}

func PrintRankedHands(hands [6]Hand) {
    fmt.Println("--- WINNING HAND ORDER ---")

    for _, hand := range hands {
        fmt.Printf("%s - %s\n", hand.String(), HAND_MAP[hand.handType])
    }
}
