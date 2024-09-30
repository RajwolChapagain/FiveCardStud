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

bool HandIdentifier::isRoyalStraightFlush(const vector<Card>& sortedCardList) {
    if (isRoyalStraight(sortedCardList) && isFlush(sortedCardList))
        return true;

    return false;
}

bool HandIdentifier::isRoyalStraight(const vector<Card>& sortedCardList) {
    if (sortedCardList[0].getValue() == 0 && sortedCardList[1].getValue() == 9 &&
            sortedCardList[2].getValue() == 10 && sortedCardList[3].getValue() == 11 &&
            sortedCardList[4].getValue() == 12)
        return true;

    return false;
}

bool HandIdentifier::isStraightFlush(const vector<Card>& sortedCardList) {
    if (isStraight(sortedCardList) && isFlush(sortedCardList))
        return true;

    return false;
}

bool HandIdentifier::isStraight(const vector<Card>& sortedCardList) {
    if (isRoyalStraight(sortedCardList))
        return true;

    int prevValue = sortedCardList[0].getValue();

    for (int i = 1; i < sortedCardList.size(); i++) {
        int currentValue = sortedCardList[i].getValue();
        if (currentValue != prevValue + 1)
            return false;

        prevValue = currentValue;
    }

    return true;
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
