#ifndef HAND_SORTER_H
#define HAND_SORTER_H

#include <vector>
#include <functional>
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

        static int compareHighestCard(const vector<Card>& cardList1, const vector<Card>& cardList2);
};

#endif
