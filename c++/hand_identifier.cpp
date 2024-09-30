#include <vector>
#include <algorithm>
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

bool HandIdentifier::isFourOfAKind(const vector<Card>& sortedCardList) {
    vector<int> frequencySet = getFrequencySet(sortedCardList);

    if (find(frequencySet.begin(), frequencySet.end(), 4) != frequencySet.end())
        return true;

    return false;
}

bool HandIdentifier::isFullHouse(const vector<Card>& sortedCardList) {
    vector<int> frequencySet = getFrequencySet(sortedCardList);

    if (find(frequencySet.begin(), frequencySet.end(), 3) != frequencySet.end() &&
         find(frequencySet.begin(), frequencySet.end(), 2) != frequencySet.end())
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

bool HandIdentifier::isThreeOfAKind(const vector<Card>& sortedCardList) {
    vector<int> frequencySet = getFrequencySet(sortedCardList);

    if (find(frequencySet.begin(), frequencySet.end(), 3) != frequencySet.end())
        return true;

    return false;
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

//Returns a vector of integers containing the frequency of each unique card
//Ex: Hand: JD, JH, 3S, 3C, AH
//    returns: {2, 2, 1}
vector<int> HandIdentifier::getFrequencySet(const vector<Card>& sortedCardList) {
    vector<int> frequencySet;
    
    int prevValue = -1;
    for (const Card& c : sortedCardList) {
        if (c.getValue() == prevValue) //If a card of this value has been counted, move on
            continue;

        frequencySet.push_back(getCardFrequency(c, sortedCardList));
        prevValue = c.getValue();
    }

    sort(frequencySet.begin(), frequencySet.end());

    return frequencySet;
}

int HandIdentifier::getCardFrequency(const Card& card, const vector<Card>& sortedCardList) {
    int frequency = 0;

    for (const Card& c : sortedCardList) {
        if (c.getValue() == card.getValue())
            frequency += 1;
    }

    return frequency;
}
