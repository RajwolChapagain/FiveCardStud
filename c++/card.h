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
        static const vector<string> valueMap;
        static const vector<string> suitMap;

        //Methods
        static int getValueIndex(string s);
        static int getSuitIndex(string s);

    public:
        //Constructors
        Card(int value, int suit);
        Card(string valueString, string suitString);

        friend std::ostream& operator<<(std::ostream& os, const Card& c);

        //Methods
        int getValue() const;
        int getSuit() const;       
};

#endif
