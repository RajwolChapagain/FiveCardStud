#include <vector>
#include <algorithm>
#include <functional>
#include "hand.h"
#include "hand_sorter.h"

using namespace std;

vector<function<bool(const Hand&, const Hand&)>> HandSorter::comparators = {
    compareRoyalFlush, compareRoyalFlush, compareRoyalFlush, compareRoyalFlush, compareRoyalFlush,
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

bool HandSorter::compareRoyalFlush(const Hand& h1, const Hand& h2) {
    vector<Card> cardList1 = h1.getSortedCards();
    vector<Card> cardList2 = h2.getSortedCards();

    return cardList1[0].getSuit() > cardList2[0].getSuit();
}
