function assign_type(hand::Hand)
    cards = get_sorted_cards(hand)

    if is_royal_straight_flush(cards)
        set_type(hand, 10)
    elseif is_flush(cards)
        set_type(hand, 6)
    else
        set_type(hand, 1)
    end
end

function is_royal_straight_flush(cards::Vector{Card})
    if is_royal_straight(cards) && is_flush(cards)
        return true
    end

    return false
end

function is_royal_straight(cards::Vector{Card})
    if cards[1].value == 1 && cards[2].value == 10 && cards[3].value == 11 && cards[4].value == 12 && cards[5].value == 13
        return true
    end

    return false
end

function is_flush(cards::Vector{Card})
    prev_suit = cards[1].suit

    for card in cards
        if card.suit != prev_suit
            return false
        end
    end

    return true
end
