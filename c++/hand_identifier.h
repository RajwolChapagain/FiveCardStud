#ifndef HAND_IDENTIFIER_H
#define HAND_IDENTIFIER_H

#include <vector>
#include "hand.h"

class HandIdentifier {
    public:
        static void assignType(Hand& h);
        static bool isRoyalStraightFlush(const vector<Card>& sortedCardList);
        static bool isRoyalStraight(const vector<Card>& sortedCardList);
        static bool isStraightFlush(const vector<Card>& sortedCardList);
        static bool isFourOfAKind(const vector<Card>& sortedCardList);
        static bool isFullHouse(const vector<Card>& sortedCardList);
        static bool isFlush(const vector<Card>& sortedCardList);
        static bool isStraight(const vector<Card>& sortedCardList);
        static bool isThreeOfAKind(const vector<Card>& sortedCardList);
        static bool isTwoPair(const vector<Card>& sortedCardList);
        static bool isPair(const vector<Card>& sortedCardList);
        static vector<int> getFrequencySet(const vector<Card>& sortedCardList);
        static int getCardFrequency(const Card& card, const vector<Card>& sortedCardList);
};

#endif
