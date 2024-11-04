package main

import (
    "sort"
)

func AssignType(h *Hand) {
    cards := h.GetSortedCards()

    if IsRoyalStraightFlush(cards) {
        h.handType = 9
    } else if IsStraightFlush(cards) {
        h.handType = 8
    } else if IsFourOfAKind(cards) {
        h.handType = 7
    } else if IsFullHouse(cards) {
        h.handType = 6
    } else if IsFlush(cards) {
        h.handType = 5
    } else if IsStraight(cards) {
        h.handType = 4
    } else if IsThreeOfAKind(cards) {
        h.handType = 3
    } else {
        h.handType = 0
    }
}

func IsRoyalStraightFlush(cards []Card) bool {
    if IsRoyalStraight(cards) && IsFlush(cards) {
        return true
    }

    return false
}

func IsRoyalStraight(cards []Card) bool {
    if cards[0].value == 0 && cards[1].value == 9 && cards[2].value == 10 && cards[3].value == 11 && cards[4].value == 12 {
        return true
    }

    return false
}

func IsStraightFlush(cards []Card) bool {
    if IsStraight(cards) && IsFlush(cards) {
        return true
    }

    return false
}


func IsFourOfAKind(cards []Card) bool {
    set := GetFrequencySet(cards)

    for _, v := range set {
        if v == 4 {
            return true
        }
    }

    return false
}

func IsFullHouse(cards []Card) bool {
    set := GetFrequencySet(cards)

    if set[0] == 2 && set[1] == 3 {
        return true
    }

    return false
}

func IsFlush(cards []Card) bool {
    prevSuit := cards[0].suit

    for _, card := range cards {
        if card.suit != prevSuit {
            return false
        }
    }

    return true
}

func IsStraight(cards []Card) bool {
    if IsRoyalStraight(cards) {
        return true
    }

    prevValue := cards[0].value - 1

    for _, card := range cards {
        currValue := card.value

        if currValue != prevValue + 1 {
            return false
        }

        prevValue = currValue
    }

    return true
}

func IsThreeOfAKind(cards []Card) bool {
    set := GetFrequencySet(cards)

    for _, v := range set {
        if v == 3 {
            return true
        }
    }

    return false
}

// =============== Helper ===============

func GetFrequencySet(cards []Card) []int {
    set := []int{}

    prevValue := -1

    for _, card := range cards {
        currValue := card.value

        if currValue == prevValue {
            continue
        }

        count := 0

        for _, c := range cards {
            if c.value == currValue {
                count += 1
            }
        }

        set = append(set, count)
        prevValue = currValue
    }

    sort.Ints(set)

    return set
}
