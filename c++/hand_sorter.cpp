#include <vector>
#include <algorithm>
#include <functional>
#include "hand.h"
#include "hand_identifier.h"
#include "hand_sorter.h"

using namespace std;

vector<function<bool(const Hand&, const Hand&)>> HandSorter::comparators = {
    compareRoyalFlush, compareStraightFlush, compareRoyalFlush, compareRoyalFlush, compareRoyalFlush,
    compareRoyalFlush, compareRoyalFlush, compareRoyalFlush, compareRoyalFlush, compareRoyalFlush
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
            if (HandIdentifier::isStraight(cardList1))
                valueList1.push_back(0);
            else
                valueList1.push_back(13);
        }
        else
            valueList1.push_back(cardList1[i].getValue());

        //Push value for second list
        if (cardList2[i].getValue() == 0) {
            if (HandIdentifier::isStraight(cardList2))
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
