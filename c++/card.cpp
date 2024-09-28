#include <iostream>
#include <iomanip>
#include <vector>
#include <stdexcept>
#include "card.h"

using namespace std;

const vector<string> Card::VALUE_MAP = {"A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"};
const vector<string> Card::SUIT_MAP = {"D", "C", "H", "S"}; 

Card::Card(int value, int suit): value(value), suit(suit) {
    if (value < 0)
        throw invalid_argument("Value cannot be less than 0");
    else if (value >= VALUE_MAP.size())
        throw invalid_argument("Value cannot be larger than or equal to " + to_string(VALUE_MAP.size()));

    if (suit < 0)
        throw invalid_argument("Suit cannot be less than 0");
    else if (suit >= SUIT_MAP.size())
        throw invalid_argument("Suit cannot be larger than or equal to " + to_string(SUIT_MAP.size()));
}

Card::Card(string valueString, string suitString) : Card(getValueIndex(valueString), getSuitIndex(suitString)) {}

ostream& operator<<(ostream& os, const Card& c) {
    int printWidth = 4;
    string printString = Card::VALUE_MAP[c.value] + Card::SUIT_MAP[c.suit];
    os << left << setw(printWidth) << printString;
    return os;
}

//=============== Private Methods ===============
int Card::getValueIndex(string s) {
    for (int i = 0; i < Card::VALUE_MAP.size(); i++)
        if (s == Card::VALUE_MAP[i])
            return i;

    return -1;
}

int Card::getSuitIndex(string s) {
    for (int i = 0; i < Card::SUIT_MAP.size(); i++)
        if (s == Card::SUIT_MAP[i])
            return i;

    return -1;
}

//=============== Public Methods ===============
int Card::getValue() const {
    return value;
}

int Card::getSuit() const {
    return suit;
}
