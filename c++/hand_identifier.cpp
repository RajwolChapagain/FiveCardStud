#include <vector>
#include "card.h"
#include "hand.h"
#include "hand_identifier.h"

void HandIdentifier::assignType(Hand& h) {
    vector<Card> sortedCardList = h.getSortedCards();

    if (isFlush(sortedCardList))
        h.setType(Hand::FLUSH);

    h.setType(Hand::HIGH_CARD);
}

bool HandIdentifier::isFlush(const vector<Card>& sortedCardList) {
    int prevSuit = sortedCardList[0].getSuit();

    for (const Card& c: sortedCardList) {
        if (c.getSuit() != prevSuit)
            return false;
        
        prevSuit = c.getSuit();
    }

    return true;
}


bool HandIdentifier::isRoyalStraight(const vector<Card>& sortedCardList) {
    if (sortedCardList[0].getValue() == 0 && sortedCardList[1].getValue() == 9 &&
            sortedCardList[2].getValue() == 10 && sortedCardList[3].getValue() == 11 &&
            sortedCardList[4].getValue() == 12)
        return true;

    return false;
}
