function sort_hands(hands::Vector{Hand})
    sort!(hands, by = hand -> hand.type, rev = true)
end
