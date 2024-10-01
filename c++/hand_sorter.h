#ifndef HAND_SORTER_H
#define HAND_SORTER_H

#include <vector>
#include "hand.h"

class HandSorter {
    public:
        static void sortHands(vector<Hand>& hands);
        static void sortHandsByType(vector<Hand>& hands);
};

#endif
