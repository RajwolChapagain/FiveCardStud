class Card:
    VALUE_MAP = ["A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"]
    SUIT_MAP = ["D", "C", "H", "S"]

    def __init__(self, value, suit):
        self.value = value
        self.suit = suit

    def __str__(self):
        cardString = Card.VALUE_MAP[self.getValue()] + Card.SUIT_MAP[self.getSuit()]
        return f"{cardString : >3}"

    def getValue(self):
        return self.value

    def getSuit(self):
        return self.suit
