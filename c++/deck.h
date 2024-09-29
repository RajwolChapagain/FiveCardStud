#ifndef DECK_H
#define DECK_H

#include <iostream>
#include <vector>
#include "card.h"

using namespace std;

class Deck {
    private:
        vector<Card> cards;

        static int NUM_CARDS;

        void shuffle();

    public:
        Deck();
        
        friend ostream& operator<<(ostream& os, const Deck& d);

        void printInOneLine() const;
        Card dealCard();
};

#endif
