import sys
from card import Card
from deck import Deck
from hand import Hand

def main():
    is_testing = len(sys.argv) > 1
    hands = []

    print('*** P O K E R   H A N D   A N A L Y Z E R ***\n\n')

    if is_testing:
        pass
    else:
        deck = Deck()
        print_deck(deck)
        deal_from_deck(hands, deck)
        print_hands(hands)
        print_remaining_deck(deck)

#=============== Non-testing functions ===============

def print_deck(deck):
    print('*** USING RANDOMIZED DECK OF CARDS ***\n')

    print ('*** Shuffled 52 card deck:')
    print(deck)

def deal_from_deck(hands, deck):
    NUM_HANDS  = 6

    for i in range(NUM_HANDS):
        hands.append(Hand())

    for i in range(Hand.HAND_SIZE):
        for j in range(NUM_HANDS):
            hands[j].add_card(deck.deal_card())

def print_hands(hands):
    print('*** Here are the six hands...')

    for hand in hands:
        print(hand)
    print()

def print_remaining_deck(deck):
    print('*** Here is what remains in the deck...')
    print(repr(deck))

if __name__ == '__main__':
    main()
