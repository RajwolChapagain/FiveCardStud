package main

func AssignType(h *Hand) {
    cards := h.GetSortedCards()

    if IsRoyalStraightFlush(cards) {
        h.handType = 9
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

func IsFlush(cards []Card) bool {
    prevSuit := cards[0].suit

    for _, card := range cards {
        if card.suit != prevSuit {
            return false
        }
    }

    return true
}
