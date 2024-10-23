const HAND_SIZE = 5
const HAND_MAP =  ["High Card", "Pair", "Two Pair", "Three of a Kind", "Straight",
                "Flush", "Full House", "Four of a Kind", "Straight Flush", "Royal Straight Flush"]

struct Hand
    cards :: Vector{Card}
    type :: Int64
end

function Hand()
    hand = Hand(Vector{Card}(undef, 0), -1)
    return hand
end

function add_card(hand, card)
    push!(hand.cards, card)
end

function Base.show(io::IO, hand::Hand)
    for card in hand.cards
        print(card)
    end
end
