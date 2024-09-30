#ifndef HAND_IDENTIFIER_H
#define HAND_IDENTIFIER_H

#include <vector>
#include "hand.h"

class HandIdentifier {
    public:
        static void assignType(Hand& h);
        static bool isFlush(const vector<Card>& sortedCardList);
        static bool isRoyalStraight(const vector<Card>& sortedCardList);
};

#endif
