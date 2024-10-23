include("card.jl")
include("hand.jl")

println("*** P O K E R   H A N D   A N A L Y Z E R ***\n\n")

# =============== Non-testing functions ===============
#
function init_deck(deck :: Vector{Card})
    for i in eachindex(deck) 
        deck[i] = Card(((i - 1) % 13) + 1, ceil(i / 13))
    end

    # Shuffle deck
    for i in eachindex(deck)
        rand_index = rand(1:52)

        temp = deck[i]
        deck[i] = deck[rand_index]
        deck[rand_index] = temp
    end
end

function print_deck(deck :: Vector{Card})
    println("*** USING RANDOMIZED DECK OF CARDS ***\n")

    println("*** Shuffled 52 card deck:")
    for (i, card) in enumerate(deck)
        print(card)

        if i % 13 == 0
            println()
        end
    end
    println()
end

function deal_from_deck(hands :: Vector{Hand}, deck :: Vector{Card})
    for i in 1:6
        for hand in hands
            add_card(hand, popfirst!(deck))
        end
    end
end

function print_remaining_deck(deck :: Vector{Card})
    println("*** Here is what remains in the deck...")
    for card in deck
        print(card)
    end
    println()
end

# =============== Testing functions ===============

# =============== Common functions ===============

function print_hands(hands :: Vector{Hand})
    println("*** Here are the six hands...")

    for hand in hands
        print(hand)
        println()
    end
    println()
end

# =============== Logic ===============
hands = [Hand() for _ in 1:6]

if length(ARGS) == 1
    println("*** USING TEST DECK ***\n")
else
    deck = Vector{Card}(undef, 52)
    init_deck(deck)
    print_deck(deck)
    deal_from_deck(hands, deck)
    print_hands(hands)
    print_remaining_deck(deck)
end
