#include <iostream>
#include "deck.h"
#include "card.h"

using namespace std;

int Deck::NUM_CARDS = 52;

Deck::Deck() {
    for (int i =0; i < Deck::NUM_CARDS; i++)
        cards.push_back(Card(i % 13, i / 13));
}

ostream& operator<<(ostream& os, const Deck& d) {
    const int CARDS_PER_LINE = 13;

    for (int i =0; i < d.cards.size(); i++)
    {
        os << d.cards[i];

        if ((i + 1) % CARDS_PER_LINE == 0)
            os << endl;
    }

    return os;
}

void Deck::printInOneLine() const {
    for (int i = 0; i < cards.size(); i++)
        cout << cards[i];

    cout << endl;
}

