#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include "deck.h"
#include "hand.h"
#include "hand_identifier.h"
#include "hand_sorter.h"

using namespace std;

void printFile(const string& filePath);
void dealFromFile(vector<Hand>& hands, const string& filePath, int tokenSize);
void convertStringToHand(const string& s, Hand& h, int tokenSize);

void printDeck(const Deck& d);
void dealFromDeck(vector<Hand>& hands, Deck& d);
void printHands(const vector<Hand>& hands);
void printRemainingDeck(const Deck& d);
void assignTypes(vector<Hand>& hands);
void printRankedHands(const vector<Hand>& hands);

int main(int argc, char *argv[]) {
    bool isTesting = (argc == 2);
    const int NUM_HANDS = 6;
    vector<Hand> hands(NUM_HANDS);

    cout << "*** P O K E R   H A N D   A N A L Y Z E R ***" << endl << endl << endl;

    if (isTesting) {
        string filePath = argv[1];
        printFile(filePath);

        const int TOKEN_SIZE = 3; //Size of each comma-separated token in file

        dealFromFile(hands, filePath, TOKEN_SIZE);
        printHands(hands);
        assignTypes(hands);
        HandSorter::sortHands(hands);
        printRankedHands(hands);
    }
    else {
        Deck deck;
        printDeck(deck);
        dealFromDeck(hands, deck);
        printHands(hands);
        printRemainingDeck(deck);
        assignTypes(hands);
        HandSorter::sortHands(hands);
        printRankedHands(hands);
    }

    return 0;
}

void printFile(const string& filePath) {
    cout << "*** USING TEST DECK ***" << endl << endl;
    
    cout << "*** File: " << filePath << endl;

    ifstream f(filePath);
    string line;
    while (getline(f,line))
        cout << line << endl;
    cout << endl;

    f.close();
}

void dealFromFile(vector<Hand>& hands, const string& filePath, int tokenSize) {
    ifstream f(filePath);

    string line;
    for (int i = 0; i < hands.size(); i++) {
        getline(f, line);
        convertStringToHand(line, hands[i], tokenSize);
    }
    
    f.close();
}

void convertStringToHand(const string& s, Hand& h, int tokenSize) {
    int startIndex = 0;

    while (startIndex + tokenSize <= s.size()) {
        string valueString, suitString;  //valueString: "A", suitString: "D"
        
        string token = s.substr(startIndex, tokenSize); //token: " AD"

        suitString = token.substr(token.size() - 1, 1);
        valueString = token.substr(0, 2);

        int spaceIndex = valueString.find_first_of(' ');

        if (spaceIndex != string::npos) //If the valueString has a preceding space
            valueString.erase(valueString.begin());

        h.addCard(Card(valueString, suitString));
        startIndex += tokenSize + 1;
    }
}

void printDeck(const Deck& d) {
    cout << "*** USING RANDOMIZED DECK OF CARDS ***" << endl << endl;

    cout << "*** Shuffled 52 card deck:" << endl;
    cout << d << endl;
}

void dealFromDeck(vector<Hand>& hands, Deck& d) {
    for (int i = 0; i < Hand::HAND_SIZE; i++)
        for (int j = 0; j < hands.size(); j++)
            hands[j].addCard(d.dealCard());
}

void printHands(const vector<Hand>& hands) {
    cout << "*** Here are the six hands..." << endl;
    for (Hand h: hands)
        cout << h << endl;
    cout << endl;
}

void printRemainingDeck(const Deck& d) {
    cout << "*** Here is what remains in the deck..." << endl;
    d.printInOneLine();
    cout << endl;
}

void assignTypes(vector<Hand>& hands) {
    for (int i = 0; i < hands.size(); i++)
        HandIdentifier::assignType(hands[i]);
}

void printRankedHands(const vector<Hand>& hands) {
    cout << "--- WINNING HAND ORDER ---" << endl;
    for (const Hand& hand : hands)
        cout << hand << " - " << Hand::HAND_MAP[hand.getType()] << endl;
    cout << endl;
}
