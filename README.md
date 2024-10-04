# FiveCardStud

This repository contains a program written for Dr. Pound's CSC 330 class in various languages. The program is called FiveCardStud because it simulates a game of the Five Card Stud variation of Poker by randomizing a deck of cards, dealing six hands, and then ranking them.

To run this program, first clone the repository using the following command:
```
git clone https://anvil.cs.mercer.edu/chapagain_r/fivecardstud.git

```
### Java
1. Navigate to the java directory:
```
cd fivecardstud/java
```
2. Compile the code:
```
javac Main.java
```
3. Run it:
```
java Main
```

To run the program using test files, place them into the fivecardstud/handsets directory. One is provided for you. Pass the relative path of the appropriately formatted test file as the first command line argument while running the program like so:
```
java Main ../handsets/test
```
### C#
1. Navigate to the c# directory:
```
cd fivecardstud/c#
```
2. Compile the code:
```
mcs Poker.cs Card.cs Deck.cs Hand.cs HandAnalyzer.cs
```
3. Run it:
```
mono Poker.exe
```
To use a test set of hands, pass the relative path of the appropriately formatted test file as the first command-line argument to the program like so:
```
mono Poker.exe ../handsets/test
```

### C++
1. Navigate to the c++ directory:
```
cd fivecardstud/c++
```
2. Compile the code:
```
g++ main.cpp card.cpp deck.cpp hand.cpp hand_identifier.cpp hand_sorter.cpp
```
3. Run it:
```
./a.out
```
To use a test set of hands, pass the relative path of the appropriately formatted test file as the first command-line argument to the program like so:
```
./a.out ../handsets/test
```


