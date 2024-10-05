import sys
from deck import Deck

def main():
    is_testing = len(sys.argv) > 1

    print('*** P O K E R   H A N D   A N A L Y Z E R ***\n\n')

    if is_testing:
        pass
    else:
        deck = Deck()
        print_deck(deck)

#=============== Non-testing functions ===============

def print_deck(deck):
    print('*** USING RANDOMIZED DECK OF CARDS ***\n')

    print ('*** Shuffled 52 card deck:')
    print(deck)

if __name__ == '__main__':
    main()
