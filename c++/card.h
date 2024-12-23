#ifndef CARD_H
#define CARD_H

#include <iostream>
#include <vector>
#include <string>

using namespace std;

class Card {
    private:
        //Fields
        int value;
        int suit;

        //Static fields
        static const vector<string> VALUE_MAP;
        static const vector<string> SUIT_MAP;

        //Static Methods
        static int getValueIndex(string s);
        static int getSuitIndex(string s);

    public:
        //Constructors
        Card(int value, int suit);
        Card(string valueString, string suitString);

        friend ostream& operator<<(ostream& os, const Card& c);
        friend bool operator<(const Card& card1, const Card& card2);
        friend bool operator>(const Card& card1, const Card& card2);

        //Methods
        int getValue() const;
        int getSuit() const;       
        string getRawString() const;
};

#endif
