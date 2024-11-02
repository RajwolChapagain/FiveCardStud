function sort_hands(hands::Vector{Hand})
    sort!(hands, by = hand -> hand.type, rev = true)

    start_index = 1
    last_type = hands[1].type

   
    for (i, hand) in enumerate(hands)
        if hand.type != last_type
            hands[start_index:i-1] = sort(hands[start_index:i-1], lt=comparators[last_type], rev=true)

            start_index = i
            last_type = hand.type
        elseif i == length(hands)
            hands[start_index:length(hands)] = sort(hands[start_index:length(hands)], lt=comparators[last_type], rev=true)
        end
    end
    
end

# =============== Comparators ===============
# All return true if first hand is weaker

function compare_royal_flush(h1, h2)
    if get_sorted_cards(h1)[1].suit < get_sorted_cards(h2)[1].suit
        return true
    end

    return false
end

function compare_straight_flush(h1, h2)
    l1 = get_sorted_cards(h1)
    l2 = get_sorted_cards(h2)
    
    highest_card_comparison = compare_highest_card(l1, l2)

    if highest_card_comparison == 1
        return true
    elseif highest_card_comparison == -1
        return false
    end

    if l1[1].suit < l2[1].suit
        return true
    end

    return false
end

function compare_four_of_a_kind(h1, h2)
    c1 = get_cards_occuring_n_times(get_sorted_cards(h1), 4)[1]
    c2 = get_cards_occuring_n_times(get_sorted_cards(h2), 4)[1]

    value1 = c1.value
    value2 = c2.value

    if value1 == 1
        value1 = 14
    end

    if value2 == 1
        value2 = 14
    end

    if value1 < value2
        return true
    end

    return false
end

function compare_full_house(h1, h2)
    c1 = get_cards_occuring_n_times(get_sorted_cards(h1), 3)[1]
    c2 = get_cards_occuring_n_times(get_sorted_cards(h2), 3)[1]

    value1 = c1.value
    value2 = c2.value

    if value1 == 1
        value1 = 14
    end

    if value2 == 1
        value2 = 14
    end

    if value1 < value2
        return true
    end

    return false
end

function compare_flush(h1, h2)
    l1 = get_sorted_cards(h1)
    l2 = get_sorted_cards(h2)

    highest_card_comparison = compare_highest_card(l1, l2)

    if highest_card_comparison == 1
        return true
    elseif highest_card_comparison == -1
        return false
    end

    if l1[1].suit < l2[1].suit
        return true
    end

    return false
end

function compare_straight(h1, h2)
    l1 = get_sorted_cards(h1)
    l2 = get_sorted_cards(h2)

    highest_card_comparison = compare_highest_card(l1, l2)

    if highest_card_comparison == 1
        return true
    elseif highest_card_comparison == -1
        return false
    end

    highest_card_suit1 = -1
    highest_card_suit2 = -1

    if !is_royal_straight(l1)
        highest_card_suit1 = l1[length(l1)].suit
    else
        highest_card_suit1 = l1[1].suit
    end

    if !is_royal_straight(l2)
        highest_card_suit2 = l2[length(l2)].suit
    else
        highest_card_suit2 = l2[1].suit
    end

    if highest_card_suit1 < highest_card_suit2
        return true
    end

    return false
end

function compare_three_of_a_kind(h1, h2)
    c1 = get_cards_occuring_n_times(get_sorted_cards(h1), 3)[1]
    c2 = get_cards_occuring_n_times(get_sorted_cards(h2), 3)[1]

    value1 = c1.value
    value2 = c2.value

    if value1 == 1
        value1 = 14
    end

    if value2 == 1
        value2 = 14
    end

    if value1 < value2
        return true
    end

    return false
end

comparators = [compare_royal_flush, compare_royal_flush, compare_royal_flush, compare_three_of_a_kind, compare_straight, compare_flush, compare_full_house, compare_four_of_a_kind, compare_straight_flush, compare_royal_flush] 


# =============== Helpers ===============

# Returns:
#  1 if l1 is weaker
# -1 if l2 is weaker
#  0 if both are tied
function compare_highest_card(l1, l2)
    value_list1 = []
    value_list2 = []

    for (card1, card2) in zip(l1, l2)
        if card1.value == 1
            if length(l1) == HAND_SIZE
                if is_straight(l1) && !is_royal_straight(l1)
                    push!(value_list1, 1)
                else
                    push!(value_list1, 14)
                end
            else
                push!(value_list1, 14)
            end
        else
            push!(value_list1, card1.value)
        end

        if card2.value == 1
            if length(l2) == HAND_SIZE
                if is_straight(l2) && !is_royal_straight(l2)
                    push!(value_list2, 1)
                else
                    push!(value_list2, 14)
                end
            else
                push!(value_list2, 14)
            end
        else
            push!(value_list2, card2.value)
        end
    end

    sort!(value_list1, rev=true)
    sort!(value_list2, rev=true)

    for (value1, value2) in zip(value_list1, value_list2)
        if value1 > value2
            return -1
        elseif value1 < value2
            return 1
        end
    end

    return 0
end

function get_cards_occuring_n_times(card_list, n)
    result = []
    occurence = 1
    last_val = card_list[1].value

    for card in card_list
        if !(card in result)
            if count(x -> x == card, card_list) == n
                push!(result, card)
            end
        end
    end

    return result
end
