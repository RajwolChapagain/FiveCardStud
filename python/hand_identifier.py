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

    def is_straight_flush(cards):
        if HandIdentifier.is_straight(cards) and HandIdentifier.is_flush(cards):
            return True

        return False

    def is_four_of_a_kind(cards):
        if 4 in HandIdentifier.get_frequency_set(cards):
            return True

        return False

    def is_full_house(cards):
        frequency_set = HandIdentifier.get_frequency_set(cards)

        if 3 in frequency_set and 2 in frequency_set:
            return True

        return False

    def is_flush(cards):
        prev_suit = cards[0].get_suit()

        for card in cards:
            if card.get_suit() != prev_suit:
                return False

            prev_suit = card.get_suit()

        return True

    def is_straight(cards):
        if HandIdentifier.is_royal_straight(cards):
            return True

        prev_value = cards[0].get_value() - 1

        for card in cards:
            curr_value = card.get_value()

            if curr_value != prev_value + 1:
                return False

            prev_value = curr_value

        return True

    def is_three_of_a_kind(cards):
        if 3 in HandIdentifier.get_frequency_set(cards):
            return True

        return False

#=============== Helper methods ===============  
    def get_frequency_set(cards):
        frequency_set = []

        prev_value = -1
        for card in cards:
            if card.get_value() == prev_value:
                continue

            frequency_set.append(cards.count(card))
            prev_value = card.get_value()

        return sorted(frequency_set)
