class HandIdentifier:
    def assign_type(hand):
        pass

    def is_royal_straight_flush(cards):
        if HandIdentifier.is_royal_straight(cards) and HandIdentifier.is_flush(cards):
            return True

        return False

    def is_royal_straight(cards):
        if cards[0].get_value() == 0 and cards[1].get_value() == 9 and cards[2].get_value() == 10 and cards[3].get_value() == 11 and cards[4].get_value() == 12:
            return True

        return False

    def is_flush(cards):
        prev_suit = cards[0].get_suit()

        for card in cards:
            if card.get_suit() != prev_suit:
                return False

            prev_suit = card.get_suit()

        return True
