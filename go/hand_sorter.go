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

// =============== Helpers ===============

//Returns:
//  1 if l1 is stronger
// -1 if l1 is weaker
//  0 if both are equal
func CompareHighestCard(l1, l2 []Card) int {
    valueList1 := []int{}
    valueList2 := []int{}

    for i := 0; i < len(l1); i++ {
        valueList1 = append(valueList1, l1[i].value)

        if l1[i].value == 0 {
            // Treat ace as high
            valueList1[i] = 13

            // If it's a straight that doesn't end in an ace, treat ace as low
            if len(l1) == HAND_SIZE {
                if IsStraight(l1) && !IsRoyalStraight(l1) {
                    valueList1[i] = 0
                }
            }  
        }

      

        valueList2 = append(valueList2, l2[i].value)

        if l2[i].value == 0 {
            valueList2[i] = 13
 
            if len(l2) == HAND_SIZE {
                if IsStraight(l2) && !IsRoyalStraight(l2) {
                    valueList2[i] = 0
                }
            }       
        }
    }

	sort.Sort(sort.Reverse(sort.IntSlice(valueList1)))
	sort.Sort(sort.Reverse(sort.IntSlice(valueList2)))

    for i := 0; i < len(valueList1); i++ {
        if valueList1[i] > valueList2[i] {
            return 1
        } else if valueList1[i] < valueList2[i] {
            return -1
        }
    }

    return 0
}
