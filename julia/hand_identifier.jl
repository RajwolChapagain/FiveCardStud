function assign_type(hand::Hand)
    cards = get_sorted_cards(hand)

    if is_royal_straight_flush(cards)
        set_type(hand, 10)
    elseif is_straight_flush(cards)
        set_type(hand, 9)
    elseif is_four_of_a_kind(cards)
        set_type(hand, 8)
    elseif is_flush(cards)
        set_type(hand, 6)
    elseif is_straight(cards)
        set_type(hand, 5)
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

function is_straight_flush(cards::Vector{Card})
    if is_straight(cards) && is_flush(cards)
        return true
    end

    return false
end

function is_four_of_a_kind(cards::Vector{Card})
    if 4 in get_frequency_set(cards)
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

function is_straight(cards::Vector{Card})
    if is_royal_straight(cards)
        return true
    end

    prev_value = cards[1].value - 1

    for card in cards
        curr_value = card.value

        if curr_value != prev_value + 1
            return false
        end
        
        prev_value = curr_value
    end

    return true
end

# =============== Helpers ===============

function get_frequency_set(cards::Vector{Card})
    set = []

    prev_value = -1

    for card in cards
        if card.value == prev_value
            continue
        end

        push!(set, count(x -> x == card, cards))
        prev_value = card.value
    end

    return sort(set)
end
