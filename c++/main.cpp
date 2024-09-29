#include <iostream>
#include <vector>
#include "deck.h"
#include "hand.h"

using namespace std;

int main() {
    Deck deck;
    Hand hand;

    cout << "Deck before dealing:" << endl << deck << endl;

    for (int i = 0; i < Hand::HAND_SIZE; i++) 
        hand.addCard(deck.dealCard());

    cout << "Hand: " << hand << endl;

    cout << "Deck after dealing:" << endl << deck << endl;

    return 0;
}
