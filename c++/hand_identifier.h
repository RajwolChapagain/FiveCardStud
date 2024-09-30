#ifndef HAND_IDENTIFIER_H
#define HAND_IDENTIFIER_H

#include "hand.h"

class HandIdentifier {
    public:
        static void assignType(Hand& h);
        static bool isFlush(const Hand& h);
};

#endif
