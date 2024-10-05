from card import Card

class Deck:
    NUM_CARDS = 52

    def __init__(self):
        self.cards = []
        for i in range(Deck.NUM_CARDS):
            self.cards.append(Card(i % 13, int(i / 13)))
    
    def __str__(self):
        CARDS_PER_LINE = 13

        result = ''

        for i, card in enumerate(self.cards):
            result += str(card)

            if (i + 1) % CARDS_PER_LINE == 0:
                result += '\n'

        return result
