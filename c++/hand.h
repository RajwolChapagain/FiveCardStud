#ifndef HAND_H
#define HAND_H

#include <iostream>
#include <vector>
#include "card.h"

using namespace std;

class Hand {
    private:
        vector<Card> cards;

    public:
        Hand();

        friend ostream& operator<<(ostream& os, const Hand& h);
        
        static const int HAND_SIZE = 5;

        void addCard(Card c);
};

#endif
