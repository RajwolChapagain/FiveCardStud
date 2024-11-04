package main

import (
    "sort"
)

func SortHands(hands *[6]Hand) {
    handsSlice := hands[:]

    sort.Slice(handsSlice, func (i, j int) bool {
        return handsSlice[i].handType > handsSlice[j].handType
    })
}
