include("card.jl")

println("*** P O K E R   H A N D   A N A L Y Z E R ***\n\n")

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
end

if length(ARGS) == 1
    println("*** USING TEST DECK ***\n")
else
    deck = Vector{Card}(undef, 52)
    init_deck(deck)
    print_deck(deck)
end
