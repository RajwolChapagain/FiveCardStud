#include <iostream>
#include <iomanip>
#include <vector>
#include "card.h"

using namespace std;

const vector<string> Card::valueMap = {"A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"};
const vector<string> Card::suitMap = {"D", "C", "H", "S"}; 

Card::Card(int value, int suit): value(value), suit(suit) {}

Card::Card(string valueString, string suitString) : Card(getValueIndex(valueString), getSuitIndex(suitString)) {}

std::ostream& operator<<(std::ostream& os, const Card& c) {
    int printWidth = 3;
    string printString = Card::valueMap[c.value] + Card::suitMap[c.suit];
    os << std::left << std::setw(printWidth) << printString;
    return os;
}

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
