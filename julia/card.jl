const VALUE_MAP = ["A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"]
const SUIT_MAP = ["D", "C", "H", "S"]

struct Card
    value :: Int64
    suit :: Int64
end

function Card(string :: String = "AD")
    string = lstrip(string)
    value_string = string[1:length(string) - 1]
    suit_string = string[length(string) : length(string)]

    value_index = findfirst(isequal(value_string), VALUE_MAP)
    suit_index = findfirst(isequal(suit_string), SUIT_MAP)

    return Card(value_index, suit_index)
end

function Base.show(io::IO, c::Card)
    print(rpad(get_card_string(c), 4))
end

function get_card_string(c :: Card)
    return VALUE_MAP[c.value] * SUIT_MAP[c.suit]
end

function Base.isless(c1::Card, c2::Card)
    return c1.value < c2.value
end

function Base.:(==)(c1::Card, c2::Card)
    return c1.value == c2.value
end
