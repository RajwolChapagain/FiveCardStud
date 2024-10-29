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

comparators = [compare_royal_flush, compare_royal_flush, compare_royal_flush, compare_royal_flush, compare_royal_flush, compare_royal_flush, compare_royal_flush, compare_royal_flush, compare_royal_flush, compare_royal_flush] 


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
