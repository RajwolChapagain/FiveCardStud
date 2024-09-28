#include <string>

class Card {
    private:
        int value;
        int suit;
        static const std::string valueMap[];
        static const std::string suitMap[];

    public:
        Card(int value, int suit);
        friend std::ostream& operator<<(std::ostream& os, const Card& c);
};
