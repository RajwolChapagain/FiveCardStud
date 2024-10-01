#include <vector>
#include <algorithm>
#include "hand.h"
#include "hand_sorter.h"

using namespace std;

void HandSorter::sortHands(vector<Hand>& hands) {
    sortHandsByType(hands);
}

void HandSorter::sortHandsByType(vector<Hand>& hands) {
    sort(hands.begin(), hands.end(), greater<Hand>());
}
