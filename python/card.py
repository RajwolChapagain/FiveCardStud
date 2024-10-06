class Card:
    VALUE_MAP = ['A', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K']
    SUIT_MAP = ['D', 'C', 'H', 'S']

#=============== Static methods ===============

    def get_value_index(value):
        return Card.VALUE_MAP.index(value)

    def get_suit_index(suit):
        return Card.SUIT_MAP.index(suit) 

#=============== Instance methods ===============

    def __init__(self, *args):
        if len(args) == 2:
            self.value = args[0]
            self.suit = args[1]
        elif len(args) == 1:
            string = args[0].strip()
            value_string = string[:-1]
            suit_string = string[-1]
            
            self.value = Card.get_value_index(value_string)
            self.suit = Card.get_suit_index(suit_string)

    def __lt__(self, other):
        return self.get_value() < other.get_value()

    def __eq__(self, other):
        return self.get_value() == other.get_value()

    def __str__(self, PRINT_WIDTH=4):
        card_string = Card.VALUE_MAP[self.get_value()] + Card.SUIT_MAP[self.get_suit()]
        return f'{card_string : <{PRINT_WIDTH}}'

    def __repr__(self):
        class_name = self.__class__.__name__
        return f'{class_name}(value={self.get_value()}, suit={self.get_suit()})'

    def get_value(self):
        return self.value

    def get_suit(self):
        return self.suit

