#ifndef HAND_SORTER_H
#define HAND_SORTER_H

#include <vector>
#include <functional>
#include "card.h"
#include "hand.h"

using namespace std;

class HandSorter {
    private:
        static vector<function<bool(const Hand&, const Hand&)>> comparators;

    public:
        static void sortHands(vector<Hand>& hands);
        static void sortHandsByType(vector<Hand>& hands);
        static void sortTies(vector<Hand>& hands);
        static bool compareRoyalFlush(const Hand& h1, const Hand& h2);
        static bool compareStraightFlush(const Hand& h1, const Hand& h2);
        static bool compareFourOfAKind(const Hand& h1, const Hand& h2);
        static bool compareFullHouse(const Hand& h1, const Hand& h2);
        static bool compareFlush(const Hand& h1, const Hand& h2);
        static bool compareStraight(const Hand& h1, const Hand& h2);
        static bool compareThreeOfAKind(const Hand& h1, const Hand& h2);
        static bool compareTwoPair(const Hand& h1, const Hand& h2);
        static bool comparePair(const Hand& h1, const Hand& h2);
        static bool compareHighCard(const Hand& h1, const Hand& h2);

        static int compareHighestCard(const vector<Card>& cardList1, const vector<Card>& cardList2);
        static Card getHighestCard(vector<Card> cardList);
        static vector<Card> getCardOccuringNTimes(const vector<Card>& cardList, int n);
};

#endif
