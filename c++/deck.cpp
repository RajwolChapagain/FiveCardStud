#include <iostream>
#include <chrono>
#include <random>
#include "deck.h"
#include "card.h"

using namespace std;

int Deck::NUM_CARDS = 52;

Deck::Deck() {
    for (int i =0; i < Deck::NUM_CARDS; i++)
        cards.push_back(Card(i % 13, i / 13));

    shuffle();
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

//=============== Private Methods ===============
void Deck::shuffle() {
    //Initialize a random generator using current time as seed
    unsigned seed = chrono::system_clock::now().time_since_epoch().count();
    default_random_engine generator(seed);

    uniform_int_distribution<int> distribution(0, 51);
    int randInt;

    for (int i = 0; i < cards.size(); i++) {
        randInt = distribution(generator);

        Card temp = cards[i];
        cards[i] = cards[randInt];
        cards[randInt] = temp;
    }
}


//=============== Public Methods ===============
void Deck::printInOneLine() const {
    for (int i = 0; i < cards.size(); i++)
        cout << cards[i];

    cout << endl;
}

Card Deck::dealCard() {
    Card topCard = cards[0];
    cards.erase(cards.begin());
    return topCard;
}

