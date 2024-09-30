#include <vector>
#include "card.h"
#include "hand.h"
#include "hand_identifier.h"

void HandIdentifier::assignType(Hand& h) {
    if (isFlush(h))
        h.setType(Hand::FLUSH);

    h.setType(Hand::HIGH_CARD);
}

bool HandIdentifier::isFlush(const Hand& h) {
    vector<Card> sortedCardList = h.getSortedCards();

    int prevSuit = sortedCardList[0].getSuit();

    for (const Card& c: sortedCardList) {
        if (c.getSuit() != prevSuit)
            return false;
        
        prevSuit = c.getSuit();
    }

    return true;
}
