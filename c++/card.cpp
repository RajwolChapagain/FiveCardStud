#include <iostream>
#include <iomanip>
#include "card.h"

using namespace std;

const string Card::valueMap[] = {"A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"};
const string Card::suitMap[] = {"D", "C", "H", "S"}; 

Card::Card(int value, int suit): value(value), suit(suit) {}

std::ostream& operator<<(std::ostream& os, const Card& c) {
    int printWidth = 3;
    string printString = Card::valueMap[c.value] + Card::suitMap[c.suit];
    os << std::left << std::setw(printWidth) << printString;
    return os;
}
