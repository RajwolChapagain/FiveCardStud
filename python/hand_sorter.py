from functools import cmp_to_key

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
