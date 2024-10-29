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
