import copy
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
        hand_type = hands[0].get_type()
        if hand_type == 9:
            hands.sort(key=cmp_to_key(HandSorter.compare_royal_flush))
        elif hand_type == 8:
            hands.sort(key=cmp_to_key(HandSorter.compare_straight_flush))
        elif hand_type == 7:
            hands.sort(key=cmp_to_key(HandSorter.compare_four_of_a_kind))
        elif hand_type == 6:
            hands.sort(key=cmp_to_key(HandSorter.compare_full_house))
        elif hand_type == 5:
            hands.sort(key=cmp_to_key(HandSorter.compare_flush))
        elif hand_type == 4:
            hands.sort(key=cmp_to_key(HandSorter.compare_straight))
        elif hand_type == 3:
            hands.sort(key=cmp_to_key(HandSorter.compare_three_of_a_kind))
        elif hand_type == 2:
            hands.sort(key=cmp_to_key(HandSorter.compare_two_pair))
        elif hand_type == 1:
            hands.sort(key=cmp_to_key(HandSorter.compare_pair))
        else:
            hands.sort(key=cmp_to_key(HandSorter.compare_high_card))

        return hands

#=============== Comparator Methods ===============
# All return:
# -1 if first hand is stronger
#  1 if second hand is stronger

    def compare_royal_flush(h1, h2):
        if h1.get_sorted_cards()[0].get_suit() > h2.get_sorted_cards()[0].get_suit():
            return -1
        
        return 1

    def compare_straight_flush(h1, h2):
        l1 = h1.get_sorted_cards()
        l2 = h2.get_sorted_cards()

        highest_card_comparison = HandSorter.compare_highest_card(l1, l2)

        if highest_card_comparison != 0:
            return highest_card_comparison
        
        if l1[0].get_suit() > l2[0].get_suit():
            return -1
        
        return 1

    def compare_four_of_a_kind(h1, h2):
        c1 = HandSorter.get_cards_occuring_n_times(h1.get_sorted_cards(), 4)[0]
        c2 = HandSorter.get_cards_occuring_n_times(h2.get_sorted_cards(), 4)[0]

        value1 = c1.get_value()
        value2 = c2.get_value()

        if c1.get_value() == 0:
            value1 = 13

        if c2.get_value() == 0:
            value2 = 13

        if value1 > value2:
            return -1

        return 1

    def compare_full_house(h1, h2):
        c1 = HandSorter.get_cards_occuring_n_times(h1.get_sorted_cards(), 3)[0]
        c2 = HandSorter.get_cards_occuring_n_times(h2.get_sorted_cards(), 3)[0]

        value1 = c1.get_value()
        value2 = c2.get_value()

        if c1.get_value() == 0:
            value1 = 13

        if c2.get_value() == 0:
            value2 = 13

        if value1 > value2:
            return -1

        return 1

    def compare_flush(h1, h2):
        l1 = h1.get_sorted_cards()
        l2 = h2.get_sorted_cards()

        highest_card_comparison = HandSorter.compare_highest_card(l1, l2)

        if highest_card_comparison != 0:
            return highest_card_comparison
        
        if l1[0].get_suit() > l2[0].get_suit():
            return -1
        
        return 1

    def compare_straight(h1, h2):
        l1 = h1.get_sorted_cards()
        l2 = h2.get_sorted_cards()

        highest_card_comparison = HandSorter.compare_highest_card(l1, l2)

        if highest_card_comparison != 0:
            return highest_card_comparison
        
        if max(l1).get_suit() > max(l2).get_suit():
            return -1
        
        return 1

    def compare_three_of_a_kind(h1, h2):
        c1 = HandSorter.get_cards_occuring_n_times(h1.get_sorted_cards(), 3)[0]
        c2 = HandSorter.get_cards_occuring_n_times(h2.get_sorted_cards(), 3)[0]

        value1 = c1.get_value()
        value2 = c2.get_value()

        if c1.get_value() == 0:
            value1 = 13

        if c2.get_value() == 0:
            value2 = 13

        if value1 > value2:
            return -1

        return 1

    def compare_two_pair(h1, h2):
        pairs1 = HandSorter.get_cards_occuring_n_times(h1.get_sorted_cards(), 2)
        pairs2 = HandSorter.get_cards_occuring_n_times(h2.get_sorted_cards(), 2)

        highest_card_comparison = HandSorter.compare_highest_card(pairs1, pairs2)

        if highest_card_comparison != 0:
            return highest_card_comparison

        kicker1 = HandSorter.get_cards_occuring_n_times(h1.get_sorted_cards(), 1)
        kicker2 = HandSorter.get_cards_occuring_n_times(h2.get_sorted_cards(), 1)

        kicker_card_comparison = HandSorter.compare_highest_card(kicker1, kicker2)

        if kicker_card_comparison != 0:
            return kicker_card_comparison

        kicker_card1 = kicker1[0]
        kicker_card2 = kicker2[0]

        if kicker_card1.get_suit() > kicker_card2.get_suit():
            return -1

        return 1

    def compare_pair(h1, h2):
        pair1 = HandSorter.get_cards_occuring_n_times(h1.get_sorted_cards(), 2)
        pair2 = HandSorter.get_cards_occuring_n_times(h2.get_sorted_cards(), 2)

        highest_card_comparison = HandSorter.compare_highest_card(pair1, pair2)

        if highest_card_comparison != 0:
            return highest_card_comparison

        singles1 = HandSorter.get_cards_occuring_n_times(h1.get_sorted_cards(), 1)
        singles2 = HandSorter.get_cards_occuring_n_times(h2.get_sorted_cards(), 1)

        highest_single_comparison = HandSorter.compare_highest_card(singles1, singles2)

        if highest_single_comparison != 0:
            return highest_single_comparison

        highest_single1 = copy.deepcopy(max(singles1))
        highest_single2 = copy.deepcopy(max(singles2)) 

        if min(singles1).get_value() == 0:
            highest_single1 = min(singles1)
        if min(singles2).get_value() == 0:
            highest_single2 = min(singles2)

        if highest_single1.get_suit() > highest_single2.get_suit():
            return -1

        return 1

    def compare_high_card(h1, h2):
        highest_card_comparison = HandSorter.compare_highest_card(h1.get_sorted_cards(), h2.get_sorted_cards())

        if highest_card_comparison != 0:
            return highest_card_comparison
        
        highest_card1 = copy.deepcopy(max(h1.get_sorted_cards()))
        highest_card2 = copy.deepcopy(max(h2.get_sorted_cards())) 

        if min(h1.get_sorted_cards()).get_value() == 0:
            highest_card1 = min(h1.get_sorted_cards())
        if min(h2.get_sorted_cards()).get_value() == 0:
            highest_card2 = min(h2.get_sorted_cards())

        if highest_card1.get_suit() > highest_card2.get_suit():
            return -1

        return 1

#=============== Helpers ===============
    def compare_highest_card(l1, l2):
        value_list1 = []
        value_list2 = []

        for card1, card2 in zip(l1, l2):
            if card1.get_value() == 0:
                if len(l1) == Hand.HAND_SIZE:
                    if HandIdentifier.is_straight(l1): 
                        value_list1.append(0)
                    else:
                        value_list1.append(13)
                else:
                    value_list1.append(13)
            else:
                value_list1.append(card1.get_value())

            if card2.get_value() == 0:
                if len(l2) == Hand.HAND_SIZE:
                    if HandIdentifier.is_straight(l2):
                        value_list2.append(0)
                    else:
                        value_list2.append(13)
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

    def get_cards_occuring_n_times(card_list, n):
        result = []
        occurence = 1
        last_val = card_list[0].get_value()
        
        for card in card_list:
            if card not in result:
                if card_list.count(card) == n:
                    result.append(card)

        return result
