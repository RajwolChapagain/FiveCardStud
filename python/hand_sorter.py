class HandSorter:
    def sort_hands(hands):
        HandSorter.sort_by_type(hands)

    def sort_by_type(hands):
        hands.sort(key=lambda hand : hand.get_type(), reverse=True)
