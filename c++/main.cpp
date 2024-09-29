#include <iostream>
#include <vector>
#include "deck.h"
#include "hand.h"

using namespace std;

void printDeck(const Deck& d);
void dealFromDeck(vector<Hand>& hands, Deck& d);
void printHands(const vector<Hand>& hands);
void printRemainingDeck(const Deck& d);

// void printTestFile(char *fileName);

int main(int argc, char *argv[]) {
    bool isTesting = (argc == 2);
    const int NUM_HANDS = 6;
    vector<Hand> hands(NUM_HANDS);

    cout << "*** P O K E R   H A N D   A N A L Y Z E R ***" << endl << endl;

    if (isTesting) {
        // printTestFile(argv[1]);
        // dealFromFile(hands, file);
        //printHands(hands);
        //printRankedHands();
    }
    else {
        Deck deck;
        printDeck(deck);
        dealFromDeck(hands, deck);
        printHands(hands);
        printRemainingDeck(deck);
        // printRankedHands();
    }

    return 0;
}


void printDeck(const Deck& d) {
    cout << "*** USING RANDOMIZED DECK OF CARDS ***" << endl << endl;

    cout << "*** Shuffled 52 card deck:" << endl;
    cout << d << endl;
}

void dealFromDeck(vector<Hand>& hands, Deck& d) {
    for (int i = 0; i < hands.size(); i++)
        for (int j = 0; j < Hand::HAND_SIZE; j++)
            hands[j].addCard(d.dealCard());
}

void printHands(const vector<Hand>& hands) {
    cout << "*** Here are the six hands..." << endl;
    for (Hand h: hands)
        cout << h;
}

void printRemainingDeck(const Deck& d) {
    cout << "*** Here is what remains in the deck..." << endl;
    d.printInOneLine();
    cout << endl;
}

