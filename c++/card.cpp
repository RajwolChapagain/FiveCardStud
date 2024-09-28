#include <iostream>
#include "card.h"

using namespace std;

const string Card::valueMap[] = {"A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"};
const string Card::suitMap[] = {"D", "C", "H", "S"}; 

Card::Card(int value, int suit): value(value), suit(suit) {}

std::ostream& operator<<(std::ostream& os, const Card& c) {
    os << Card::valueMap[c.value] << Card::suitMap[c.suit];
    return os;
}
