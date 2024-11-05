package main

import (
    "sort"
)

var comparators = [10] func (Hand, Hand) bool {CompareHighCard, ComparePair, CompareTwoPair, CompareThreeOfAKind, CompareStraight, CompareFlush, CompareFullHouse, CompareFourOfAKind, CompareStraightFlush, CompareRoyalFlush}

func SortHands(hands *[6]Hand) {
    handsSlice := hands[:]

    sort.Slice(handsSlice, func (i, j int) bool {
        return handsSlice[i].handType > handsSlice[j].handType
    })

    startIndex := 0
    lastType := handsSlice[0].handType

    for i, hand := range handsSlice {
        if hand.handType != lastType {
            SortSubarray(hands[startIndex:i])

            startIndex = i
            lastType = hand.handType
        } else if i == len(hands) - 1 {
            SortSubarray(hands[startIndex:])
        }
    }
}

func SortSubarray(hands []Hand) {
    handType := hands[0].handType

    sort.Slice(hands, func(i, j int) bool {
        return comparators[handType](hands[i], hands[j])
    })
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

func CompareStraightFlush(h1, h2 Hand) bool {
    l1 := h1.GetSortedCards()
    l2 := h2.GetSortedCards()
    
    highestCardComparison := CompareHighestCard(l1, l2)

    if highestCardComparison == 1 {
        return true
    } else if highestCardComparison == -1 {
        return false
    }

    if l1[0].suit > l2[0].suit {
        return true
    }

    return false
}

func CompareFourOfAKind(h1, h2 Hand) bool {
    c1 := GetCardsOccuringNTimes(h1.GetSortedCards(), 4)[0]
    c2 := GetCardsOccuringNTimes(h2.GetSortedCards(), 4)[0]

    value1 := c1.value
    value2 := c2.value

    if value1 == 0 {
        value1 = 13
    }

    if value2 == 0 {
        value2 = 13
    }

    if value1 > value2 {
        return true
    }

    return false
}

func CompareFullHouse(h1, h2 Hand) bool {
    c1 := GetCardsOccuringNTimes(h1.GetSortedCards(), 3)[0]
    c2 := GetCardsOccuringNTimes(h2.GetSortedCards(), 3)[0]

    value1 := c1.value
    value2 := c2.value

    if value1 == 0 {
        value1 = 13
    }

    if value2 == 0 {
        value2 = 13
    }

    if value1 > value2 {
        return true
    }

    return false
}

func CompareFlush(h1, h2 Hand) bool {
    l1 := h1.GetSortedCards()
    l2 := h2.GetSortedCards()
    
    highestCardComparison := CompareHighestCard(l1, l2)

    if highestCardComparison == 1 {
        return true
    } else if highestCardComparison == -1 {
        return false
    }

    if l1[0].suit > l2[0].suit {
        return true
    }

    return false
}

func CompareStraight(h1, h2 Hand) bool {
    l1 := h1.GetSortedCards()
    l2 := h2.GetSortedCards()

    highestCardComparison := CompareHighestCard(l1, l2)

    if highestCardComparison == 1 {
        return true
    } else if highestCardComparison == -1 {
        return false
    }

    highestCardSuit1 := l1[len(l1) - 1].suit
    highestCardSuit2 := l2[len(l2) - 1].suit

    if IsRoyalStraight(l1) {
        highestCardSuit1 = l1[0].suit
    }

    if IsRoyalStraight(l2) {
        highestCardSuit2 = l2[0].suit
    }

    if highestCardSuit1 > highestCardSuit2 {
        return true
    }
    return false
}

func CompareThreeOfAKind(h1, h2 Hand) bool {
    c1 := GetCardsOccuringNTimes(h1.GetSortedCards(), 3)[0]
    c2 := GetCardsOccuringNTimes(h2.GetSortedCards(), 3)[0]

    value1 := c1.value
    value2 := c2.value

    if value1 == 0 {
        value1 = 13
    }

    if value2 == 0 {
        value2 = 13
    }

    if value1 > value2 {
        return true
    }

    return false
}

func CompareTwoPair(h1, h2 Hand) bool {
    l1 := h1.GetSortedCards()
    l2 := h2.GetSortedCards()

    pairs1 := GetCardsOccuringNTimes(l1, 2)
    pairs2 := GetCardsOccuringNTimes(l2, 2)

    highestCardComparison := CompareHighestCard(pairs1, pairs2)

    if highestCardComparison == 1 {
        return true
    } else if highestCardComparison == -1 {
        return false
    }

    kicker1 := GetCardsOccuringNTimes(l1, 1)
    kicker2 := GetCardsOccuringNTimes(l2, 1)

    kickerCardComparison := CompareHighestCard(kicker1, kicker2)

    if kickerCardComparison == 1 {
        return true
    } else if kickerCardComparison == -1 {
        return false
    }

    kickerCard1 := kicker1[0]
    kickerCard2 := kicker2[0]

    if kickerCard1.suit > kickerCard2.suit {
        return true
    }
    
    return false
}

func ComparePair(h1, h2 Hand) bool {
    l1 := h1.GetSortedCards()
    l2 := h2.GetSortedCards()

    pair1 := GetCardsOccuringNTimes(l1, 2)
    pair2 := GetCardsOccuringNTimes(l2, 2)
    
    highestCardComparison := CompareHighestCard(pair1, pair2)

    if highestCardComparison == 1 {
        return true
    } else if highestCardComparison == -1 {
        return false
    }

    singles1 := GetCardsOccuringNTimes(l1, 1)
    singles2 := GetCardsOccuringNTimes(l2, 1)

    highestSingleComparison := CompareHighestCard(singles1, singles2)

    if highestSingleComparison == 1 {
        return true
    } else if highestSingleComparison == -1 {
        return false
    }
    
    sort.Slice(singles1, func(i, j int) bool {
        return singles1[i].value < singles1[j].value
    })

    sort.Slice(singles2, func(i, j int) bool {
        return singles2[i].value < singles2[j].value
    })

    highestSingleSuit1 := singles1[len(singles1) - 1].suit
    highestSingleSuit2 := singles2[len(singles2) - 1].suit

    if singles1[0].value == 0 {
        highestSingleSuit1 = singles1[0].suit
    }
    if singles2[0].value == 0 {
        highestSingleSuit2 = singles2[0].suit
    }

    if highestSingleSuit1 > highestSingleSuit2 {
        return true
    }
    
    return false
}

func CompareHighCard(h1, h2 Hand) bool {
    l1 := h1.GetSortedCards()
    l2 := h2.GetSortedCards()

    highestCardComparison := CompareHighestCard(l1, l2)

    if highestCardComparison == 1 {
        return true
    } else if highestCardComparison == -1 {
        return false
    }

    highestCardSuit1 := l1[len(l1) - 1].suit
    highestCardSuit2 := l2[len(l1) - 1].suit

    if l1[0].value == 0 {
        highestCardSuit1 = l1[0].suit
    }
    if l2[0].value == 0 {
        highestCardSuit2 = l2[0].suit
    }

    if highestCardSuit1 > highestCardSuit2 {
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

func GetCardsOccuringNTimes(cardList []Card, n int) []Card {
    result := []Card{}

    for _, card := range cardList {
        if !SliceContainsCard(result, card) {
            if CountCard(cardList, card) == n {
                result = append(result, card)
            }
        }
    }

    return result
}

func SliceContainsCard(slice []Card, c Card) bool {
    for _, card := range slice {
        if card.value == c.value {
            return true
        }
    }

    return false
}

func CountCard(slice []Card, c Card) int {
    occurence := 0

    for _, card := range slice {
        if card.value == c.value {
            occurence += 1
        }
    }

    return occurence
}
