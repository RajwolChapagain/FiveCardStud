#include "hand.h"
#include "hand_identifier.h"

void HandIdentifier::assignType(Hand& h) {
    h.setType(Hand::ROYAL_STRAIGHT_FLUSH);
}
