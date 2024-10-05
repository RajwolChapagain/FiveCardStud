class Card:
    VALUE_MAP = ['A', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K']
    SUIT_MAP = ['D', 'C', 'H', 'S']

    def __init__(self, value, suit):
        self.value = value
        self.suit = suit

    def __lt__(self, other):
        return self.get_value() < other.get_value()

    def __str__(self):
        PRINT_WIDTH = 4
        card_string = Card.VALUE_MAP[self.get_value()] + Card.SUIT_MAP[self.get_suit()]
        return f'{card_string : <{PRINT_WIDTH}}'

    def __repr__(self):
        class_name = self.__class__.__name__
        return f'{class_name}(value={self.get_value()}, suit={self.get_suit()})'

    def get_value(self):
        return self.value

    def get_suit(self):
        return self.suit
