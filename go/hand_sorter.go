package main

import (
    "sort"
)

var comparators = [10] func (Hand, Hand) bool {CompareRoyalFlush, CompareRoyalFlush, CompareRoyalFlush, CompareRoyalFlush, CompareRoyalFlush, CompareRoyalFlush, CompareRoyalFlush, CompareRoyalFlush, CompareRoyalFlush, CompareRoyalFlush}

func SortHands(hands *[6]Hand) {
    handsSlice := hands[:]

    sort.Slice(handsSlice, func (i, j int) bool {
        return handsSlice[i].handType > handsSlice[j].handType
    })

    startIndex := 0
    lastType := handsSlice[0].handType

    for i, hand := range handsSlice {
        if hand.handType != lastType {
            sort.Slice(handsSlice[startIndex:i], func (i, j int) bool {
                return comparators[lastType](handsSlice[i], handsSlice[j])
            })

            startIndex = i
            lastType = hand.handType
        } else if i == len(hands) - 1 {
            sort.Slice(handsSlice[startIndex:], func (i, j int) bool {
                return comparators[lastType](handsSlice[i], handsSlice[j])
            })
        }
    }
}

// =============== Comparators ===============
// All return:
// true if first hand is stronger
// false if first hand is weaker

func CompareRoyalFlush(h1, h2 Hand) bool {
    if h1.GetSortedCards()[0].suit > h2.GetSortedCards()[0].suit {
        return true
    }
    
    return false
}
