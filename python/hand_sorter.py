from functools import cmp_to_key
from hand import Hand
from hand_identifier import HandIdentifier

class HandSorter:
    def sort_hands(hands):
        HandSorter.sort_by_type(hands)
        HandSorter.sort_ties(hands)

    def sort_by_type(hands):
        hands.sort(key=lambda hand : hand.get_type(), reverse=True)

    def sort_ties(hands):
        start_index = 0
        last_type = hands[0].get_type()
        
        for i, hand in enumerate(hands):
            if hand.get_type() != last_type:
                hands[start_index:i] = HandSorter.sort_subarray(hands[start_index:i])

                start_index = i
                last_type = hands[i].get_type()
            elif i == (len(hands) - 1):
                hands[start_index:] = HandSorter.sort_subarray(hands[start_index:])

    def sort_subarray(hands):
        if hands[0].get_type() == 9:
            hands.sort(key=cmp_to_key(HandSorter.compare_royal_flush))

        return hands

#=============== Comparator Methods ===============
# All return:
# -1 if first hand is stronger
#  1 if second hand is stronger

    def compare_royal_flush(h1, h2):
        if h1.get_sorted_cards()[0].get_suit() > h2.get_sorted_cards()[0].get_suit():
            return -1
        
        return 1

#=============== Helpers ===============
    def compare_highest_card(l1, l2):
        value_list1 = []
        value_list2 = []

        for card1, card2 in zip(l1, l2):
            if card1.get_value() == 0:
                if HandIdentifier.is_straight(l1) and len(l1) == Hand.HAND_SIZE:
                    value_list1.append(0)
                else:
                    value_list1.append(13)
            else:
                value_list1.append(card1.get_value())

            if card2.get_value() == 0:
                if HandIdentifier.is_straight(l2) and len(l2) == Hand.HAND_SIZE:
                    value_list2.append(0)
                else:
                    value_list2.append(13)
            else:
                value_list2.append(card2.get_value())

        value_list1.sort(reverse=True)
        value_list2.sort(reverse=True)

        for value1, value2 in zip(value_list1, value_list2):
            if value1 > value2:
                return -1
            elif value1 < value2:
                return 1

        return 0

