import sys
from card import Card
from deck import Deck
from hand import Hand
from hand_identifier import HandIdentifier
from hand_sorter import HandSorter

def main():
    is_testing = len(sys.argv) > 1
    NUM_HANDS = 6
    hands = []

    for i in range(NUM_HANDS):
        hands.append(Hand())

    print('*** P O K E R   H A N D   A N A L Y Z E R ***\n\n')

    if is_testing:
        file_path = sys.argv[1]
        print_file(file_path)
        deal_from_file(hands, file_path)

        if has_duplicate(hands):
            return

        print_hands(hands)
        assign_types(hands)
        HandSorter.sort_hands(hands)
        print_ranked_hands(hands)
    else:
        deck = Deck()
        print_deck(deck)
        deal_from_deck(hands, deck)
        print_hands(hands)
        print_remaining_deck(deck)
        assign_types(hands)
        HandSorter.sort_hands(hands)
        print_ranked_hands(hands)

#=============== Testing functions ===============
def print_file(path):
    print('*** USING TEST DECK ***\n')

    print('*** File: ' + path)

    with open(path, 'r') as f:
        print(f.read())

def deal_from_file(hands, path):
    with open(path, 'r') as f:
        for i, line in enumerate(f.readlines()):
            for token in line.split(','):
                hands[i].add_card(Card(token))

def has_duplicate(hands):
    card_hashes = []

    for hand in hands:
        for card in hand.get_sorted_cards():
            card_hash = card.get_value() * 10 + card.get_suit()

            if card_hash in card_hashes:
                print('*** ERROR - DUPLICATED CARD FOUND IN DECK ***\n')

                print(f'*** Duplicate: {card.__str__(PRINT_WIDTH=0)} ***')
                return True
            else:
                card_hashes.append(card_hash)

    return False

#=============== Non-testing functions ===============

def print_deck(deck):
    print('*** USING RANDOMIZED DECK OF CARDS ***\n')

    print ('*** Shuffled 52 card deck:')
    print(deck)

def deal_from_deck(hands, deck):
    for i in range(Hand.HAND_SIZE):
        for hand in hands:
            hand.add_card(deck.deal_card())

def print_remaining_deck(deck):
    print('*** Here is what remains in the deck...')
    print(repr(deck))
    print()

#=============== Common functions ===============
def print_hands(hands):
    print('*** Here are the six hands...')

    for hand in hands:
        print(hand)
    print()

def assign_types(hands):
    for hand in hands:
        HandIdentifier.assign_type(hand)

def print_ranked_hands(hands):
    print("--- WINNING HAND ORDER ---")

    for hand in hands:
        print(f'{hand} - {Hand.HAND_MAP[hand.get_type()]}')
    print()

if __name__ == '__main__':
    main()
