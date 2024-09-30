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
        enum HAND_TYPE {
            HIGH_CARD, PAIR, TWO_PAIR, THREE_OF_A_KIND, STRAIGHT,
            FLUSH, FULL_HOUSE, FOUR_OF_A_KIND, STRAIGHT_FLUSH, ROYAL_STRAIGHT_FLUSH 
        };

        void addCard(Card c);
        Hand::HAND_TYPE getType() const;
        void setType(Hand::HAND_TYPE t);
        vector<Card> getSortedCards() const;

    private:
        Hand::HAND_TYPE type; //Had to declare after declaring HAND_TYPE enum
};

#endif
