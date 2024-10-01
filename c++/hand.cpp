#include <iostream>
#include <vector>
#include <algorithm>
#include <string>
#include "hand.h"
#include "card.h"

using namespace std;

const vector<string> Hand::HAND_MAP = { "High Card", "Pair", "Two Pair", "Three of a Kind", "Straight",
                "Flush", "Full House", "Four of a Kind", "Straight Flush", "Royal Straight Flush" };

Hand::Hand() {}

ostream& operator<<(ostream& os, const Hand& h) {
    for (int i = 0; i < h.cards.size(); i++)
        os << h.cards[i];

    return os;
}

bool operator>(const Hand& hand1, const Hand& hand2) {
    return hand1.getType() > hand2.getType();
}

//=============== Public Methods ===============
void Hand::addCard(Card c) {
    cards.push_back(c);
}

Hand::HAND_TYPE Hand::getType() const {
    return type;
}

void Hand::setType(Hand::HAND_TYPE t) {
    type = t;
}

vector<Card> Hand::getSortedCards() const {
    vector<Card> sortedCards(cards);
    sort(sortedCards.begin(), sortedCards.end());

    return sortedCards;
}
