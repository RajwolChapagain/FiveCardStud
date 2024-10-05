import random
from card import Card

class Deck:
    NUM_CARDS = 52

    def __init__(self):
        self.cards = []
        for i in range(Deck.NUM_CARDS):
            self.cards.append(Card(i % 13, int(i / 13)))

        self.shuffle()
    
    def __str__(self):
        CARDS_PER_LINE = 13

        result = ''

        for i, card in enumerate(self.cards):
            result += str(card)

            if (i + 1) % CARDS_PER_LINE == 0:
                result += '\n'

        return result

    def __repr__(self):
        result = ''

        for i, card in enumerate(self.cards):
            result += str(card)

        return result

    def shuffle(self):
        for i, card in enumerate(self.cards):
            random_index = random.randrange(0, 52)

            temp = self.cards[i]
            self.cards[i] = self.cards[random_index]
            self.cards[random_index] = temp

    def deal_card(self):
        return self.cards.pop(0)


