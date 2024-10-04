#include <vector>
#include <algorithm>
#include <functional>
#include "hand.h"
#include "card.h"
#include "hand_identifier.h"
#include "hand_sorter.h"

using namespace std;

vector<function<bool(const Hand&, const Hand&)>> HandSorter::comparators = {
   compareHighCard, comparePair, compareTwoPair, compareThreeOfAKind, compareStraight, 
   compareFlush, compareFullHouse, compareFourOfAKind, compareStraightFlush, compareRoyalFlush
};

void HandSorter::sortHands(vector<Hand>& hands) {
    sortHandsByType(hands);
    sortTies(hands);
}

void HandSorter::sortHandsByType(vector<Hand>& hands) {
    sort(hands.begin(), hands.end(), greater<Hand>());
}

void HandSorter::sortTies(vector<Hand>& hands) {
    int startIndex = 0;
    int lastHandType = hands[0].getType();

    function<bool(const Hand&, const Hand&)> comparator;

    for (int i = 0; i < hands.size(); i++) {
        if (hands[i].getType() != lastHandType) {
            comparator = comparators[hands[startIndex].getType()];

            sort(hands.begin() + startIndex, hands.begin() + i, comparator);

            startIndex = i;
            lastHandType = hands[i].getType();
        }
        else if (i == hands.size() - 1) {
            comparator = comparators[hands[startIndex].getType()];

            sort(hands.begin() + startIndex, hands.end(), comparator);
        }
    }
}

//=============== Tie-Breaking Comparators ===============
//All return true is first hand is stronger
//and false if second hand is stronger

bool HandSorter::compareRoyalFlush(const Hand& h1, const Hand& h2) {
    vector<Card> cardList1 = h1.getSortedCards();
    vector<Card> cardList2 = h2.getSortedCards();

    return cardList1[0].getSuit() > cardList2[0].getSuit();
}

bool HandSorter::compareStraightFlush(const Hand& h1, const Hand& h2) {
    vector<Card> cardList1 = h1.getSortedCards();
    vector<Card> cardList2 = h2.getSortedCards();

    int highestCardComparison = compareHighestCard(cardList1, cardList2);
    if (highestCardComparison == 1)
        return false; 
    else if (highestCardComparison == -1)
        return true;
    else
        return cardList1[0].getSuit() > cardList2[0].getSuit();
}

bool HandSorter::compareFourOfAKind(const Hand& h1, const Hand& h2) {
    Card c1 = getCardOccuringNTimes(h1.getSortedCards(), 4)[0];
    Card c2 = getCardOccuringNTimes(h2.getSortedCards(), 4)[0];

    int value1 = c1.getValue();
    int value2 = c2.getValue();

    if (c1.getValue() == 0)
        value1 = 13;

    if (c2.getValue() == 0)
        value2 = 13;

    return value1 > value2;
}

bool HandSorter::compareFullHouse(const Hand& h1, const Hand& h2) {
    Card c1 = getCardOccuringNTimes(h1.getSortedCards(), 3)[0];
    Card c2 = getCardOccuringNTimes(h2.getSortedCards(), 3)[0];

    int value1 = c1.getValue();
    int value2 = c2.getValue();

    if (c1.getValue() == 0)
        value1 = 13;

    if (c2.getValue() == 0)
        value2 = 13;

    return value1 > value2;
}

bool HandSorter::compareFlush(const Hand& h1, const Hand& h2) {
    vector<Card> cardList1 = h1.getSortedCards();
    vector<Card> cardList2 = h2.getSortedCards();

    int highestCardComparison = compareHighestCard(cardList1, cardList2);
    if (highestCardComparison == 1)
        return false; 
    else if (highestCardComparison == -1)
        return true;
    else
        return cardList1[0].getSuit() > cardList2[0].getSuit();
}

bool HandSorter::compareStraight(const Hand& h1, const Hand& h2) {
    vector<Card> cardList1 = h1.getSortedCards();
    vector<Card> cardList2 = h2.getSortedCards();

    int highestCardComparison = compareHighestCard(cardList1, cardList2);
    if (highestCardComparison == 1)
        return false; 
    else if (highestCardComparison == -1)
        return true;
    else
        return cardList1.back().getSuit() > cardList2.back().getSuit();
}

bool HandSorter::compareThreeOfAKind(const Hand& h1, const Hand& h2) {
    Card c1 = getCardOccuringNTimes(h1.getSortedCards(), 3)[0];
    Card c2 = getCardOccuringNTimes(h2.getSortedCards(), 3)[0];

    int value1 = c1.getValue();
    int value2 = c2.getValue();

    if (c1.getValue() == 0)
        value1 = 13;

    if (c2.getValue() == 0)
        value2 = 13;

    return value1 > value2;
}

bool HandSorter::compareTwoPair(const Hand& h1, const Hand& h2) {
    vector<Card> pairs1 = getCardOccuringNTimes(h1.getSortedCards(), 2);
    vector<Card> pairs2 = getCardOccuringNTimes(h2.getSortedCards(), 2);

    int highestCardComparison = compareHighestCard(pairs1, pairs2);

    if (highestCardComparison == 1)
        return false;
    else if (highestCardComparison == -1)
        return true;

    vector<Card> kicker1 = getCardOccuringNTimes(h1.getSortedCards(), 1);
    vector<Card> kicker2 = getCardOccuringNTimes(h2.getSortedCards(), 1);

    int kickerCardComparison = compareHighestCard(kicker1, kicker2);

    if (kickerCardComparison == 1)
        return false;
    else if (kickerCardComparison == -1)
        return true;

    Card kickerCard1 = kicker1[0];
    Card kickerCard2 = kicker2[0];

    return kickerCard1.getSuit() > kickerCard2.getSuit();
}

bool HandSorter::comparePair(const Hand& h1, const Hand& h2) {
    vector<Card> pair1 = getCardOccuringNTimes(h1.getSortedCards(), 2);
    vector<Card> pair2 = getCardOccuringNTimes(h2.getSortedCards(), 2);

    int highestCardComparison = compareHighestCard(pair1, pair2);

    if (highestCardComparison == 1)
        return false;
    else if (highestCardComparison == -1)
        return true;

   vector<Card> singles1 = getCardOccuringNTimes(h1.getSortedCards(), 1);
   vector<Card> singles2 = getCardOccuringNTimes(h2.getSortedCards(), 1);

   int highestSingleComparison = compareHighestCard(singles1, singles2);

    if (highestSingleComparison == 1)
        return false;
    else if (highestSingleComparison == -1)
        return true;

    Card highestSingle1 = getHighestCard(singles1);
    Card highestSingle2 = getHighestCard(singles2);
    
    return highestSingle1.getSuit() > highestSingle2.getSuit();
}

bool HandSorter::compareHighCard(const Hand& h1, const Hand& h2) {
    int highestCardComparison = compareHighestCard(h1.getSortedCards(), h2.getSortedCards());

    if (highestCardComparison == 1)
        return false;
    else if (highestCardComparison == -1)
        return true;

    Card highestCard1 = getHighestCard(h1.getSortedCards());
    Card highestCard2 = getHighestCard(h2.getSortedCards());

    return highestCard1.getSuit() > highestCard2.getSuit();
}

//=============== Helpers ===============

//Returns:
// 1: If second list has higher card
//-1: If first list has higher card
// 0: If both lists have the same cards
int HandSorter::compareHighestCard(const vector<Card>& cardList1, const vector<Card>& cardList2) {
    vector<int> valueList1, valueList2;

    for (int i = 0; i < cardList1.size(); i++) {
        //Push value for first list
        if (cardList1[i].getValue() == 0) {
            if (HandIdentifier::isStraight(cardList1) && cardList1.size() == Hand::HAND_SIZE)
                valueList1.push_back(0);
            else
                valueList1.push_back(13);
        }
        else
            valueList1.push_back(cardList1[i].getValue());

        //Push value for second list
        if (cardList2[i].getValue() == 0) {
            if (HandIdentifier::isStraight(cardList2) && cardList1.size() == Hand::HAND_SIZE)
                valueList2.push_back(0);
            else
                valueList2.push_back(13);
        }
        else
            valueList2.push_back(cardList2[i].getValue());
    }

    sort(valueList1.begin(), valueList1.end(), greater<>());
    sort(valueList2.begin(), valueList2.end(), greater<>());

    for (int i = 0; i < valueList1.size(); i++) {
        if (valueList1[i] < valueList2[i])
            return 1;
        else if (valueList1[i] > valueList2[i])
            return -1;
    }

    return 0;
}

//Does not work for straights with aces as this always treats aces as high
Card HandSorter::getHighestCard(vector<Card> cardList) {
    sort(cardList.begin(), cardList.end());

    if (cardList[0].getValue() == 0)
        return cardList[0];

    return cardList.back();
}

//Expects: A sorted vector of cards
vector<Card> HandSorter::getCardOccuringNTimes(const vector<Card>& cardList, int n) {
    vector<Card> result;
    int cardOccurrence = 1;
    int lastCardValue = cardList[0].getValue();

    for (int i = 1; i < cardList.size(); i++) {
        if (cardList[i].getValue() != lastCardValue) {
            if (cardOccurrence == n)
                result.push_back(cardList[i-1]);

            cardOccurrence = 1;
            lastCardValue = cardList[i].getValue();
        }
        else
            cardOccurrence++;

        if (i == cardList.size() - 1) {
            if (cardOccurrence == n)
                result.push_back(cardList[i]);
        }
    }

    return result;
}
