include("card.jl")
include("hand.jl")
include("hand_identifier.jl")
include("hand_sorter.jl")

println("*** P O K E R   H A N D   A N A L Y Z E R ***\n\n")

# =============== Non-testing functions ===============

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
    for i in 1:5
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
    println("\n")
end

# =============== Testing functions ===============

function print_file(path :: String)
    println("*** USING TEST DECK ***\n")

    println("*** File: " * path)
    
    f = open(path, "r")
    content = read(f, String)
    print(content)
    close(f)
    println()
end

function deal_from_file(hands :: Vector{Hand}, path :: String)
    lines = readlines(path)

    for (i, line) in enumerate(lines)
        for token in split(line, ",")
            add_card(hands[i], Card(String(token)))
        end
    end
end

function has_duplicate(hands :: Vector{Hand})
    card_hashes = []

    for hand in hands
        for card in get_sorted_cards(hand)
            card_hash = card.value * 10 + card.suit
            if card_hash in card_hashes
                println("*** ERROR - DUPLICATED CARD FOUND IN DECK ***\n")

                println("*** Duplicate: ", card)
                return true
            else
                push!(card_hashes, card_hash)
            end
        end
    end

    return false
end

# =============== Common functions ===============

function print_hands(hands :: Vector{Hand})
    println("*** Here are the six hands...")

    for hand in hands
        print(hand)
        println()
    end
    println()
end

function assign_types(hands::Vector{Hand})
    for hand in hands
        assign_type(hand)
    end
end

function print_ranked_hands(hands::Vector{Hand})
    println("--- WINNING HAND ORDER ---")

    for hand in hands
        print(hand)
        println(" - ", HAND_MAP[hand.type])
    end
    println()
end

# =============== Logic ===============
hands = [Hand() for _ in 1:6]

if length(ARGS) == 1
    file_path = ARGS[1]
    print_file(file_path)
    deal_from_file(hands, file_path)

    if has_duplicate(hands)
        return
    end

    print_hands(hands)
    assign_types(hands)
    sort_hands(hands)
    print_ranked_hands(hands)
else
    deck = Vector{Card}(undef, 52)
    init_deck(deck)
    print_deck(deck)
    deal_from_deck(hands, deck)
    print_hands(hands)
    print_remaining_deck(deck)
    assign_types(hands)
    sort_hands(hands)
    print_ranked_hands(hands)
end
