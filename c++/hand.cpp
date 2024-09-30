#include <iostream>
#include <vector>
#include <algorithm>
#include "hand.h"
#include "card.h"

using namespace std;

Hand::Hand() {}

ostream& operator<<(ostream& os, const Hand& h) {
    for (int i = 0; i < h.cards.size(); i++)
        os << h.cards[i];
    os << endl;

    return os;
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