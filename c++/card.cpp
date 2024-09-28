#include <iostream>
#include <iomanip>
#include <vector>
#include <stdexcept>
#include "card.h"

using namespace std;

const vector<string> Card::valueMap = {"A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"};
const vector<string> Card::suitMap = {"D", "C", "H", "S"}; 

Card::Card(int value, int suit): value(value), suit(suit) {
    if (value < 0)
        throw invalid_argument("Value cannot be less than 0");
    else if (value >= valueMap.size())
        throw invalid_argument("Value cannot be larger than or equal to " + to_string(valueMap.size()));

    if (suit < 0)
        throw invalid_argument("Suit cannot be less than 0");
    else if (suit >= suitMap.size())
        throw invalid_argument("Suit cannot be larger than or equal to " + to_string(suitMap.size()));
}

Card::Card(string valueString, string suitString) : Card(getValueIndex(valueString), getSuitIndex(suitString)) {}

std::ostream& operator<<(std::ostream& os, const Card& c) {
    int printWidth = 3;
    string printString = Card::valueMap[c.value] + Card::suitMap[c.suit];
    os << std::left << std::setw(printWidth) << printString;
    return os;
}

//=============== Private Methods ===============
int Card::getValueIndex(string s) {
    for (int i = 0; i < Card::valueMap.size(); i++)
        if (s == Card::valueMap[i])
            return i;

    return -1;
}

int Card::getSuitIndex(string s) {
    for (int i = 0; i < Card::suitMap.size(); i++)
        if (s == Card::suitMap[i])
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
