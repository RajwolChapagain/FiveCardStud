class Hand:
    HAND_SIZE = 5
    HAND_MAP =  ['High Card', 'Pair', 'Two Pair', 'Three of a Kind', 'Straight',
                'Flush', 'Full House', 'Four of a Kind', 'Straight Flush', 'Royal Straight Flush'];
    
    def __init__(self):
        self.cards = []
        self.type = -1

    def __str__(self):
        result = ''

        for card in self.cards:
            result += str(card)

        return result

    def add_card(self, card):
        self.cards.append(card)

    def get_type(self):
        return self.type

    def set_type(self, t):
        self.type = t

    def get_sorted_cards(self):
        return sorted(self.cards)
