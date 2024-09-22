/*using java.util.List;
using java.util.List;
using java.util.Scanner;
using java.io.File;
using java.io.FileNotFoundException;
*/

using System;
using System.IO;
using System.Collections.Generic;
using FiveCardStud;

namespace FiveCardStud
{
public class Poker
{
	public static void Main(string[] args)
	{
		int NUM_HANDS = 6;
		bool isTesting = args.Length != 0;
		Hand[] handArray;
		Deck remainingDeck;

		Console.WriteLine("*** POKER HAND ANALYZER ***\n\n");

		if (isTesting)
		{
			remainingDeck = new Deck();
			handArray = new Hand[NUM_HANDS];
			Console.WriteLine("*** USING TEST DECK ***\n");
			
			string relativeFilePath = args[0];
			Console.WriteLine("*** File: " + relativeFilePath);
			try
			{
				using (StreamReader scanner = new StreamReader(relativeFilePath))
				{	
					int handNumber = 0;
					string line;
					while ((line = scanner.ReadLine()) != null)
					{
						Console.WriteLine(line);
						handArray[handNumber] = convertStringToHand(line);
						handNumber += 1;
					}
					Console.WriteLine();
				}
			}
			catch (FileNotFoundException e)
			{
				Console.WriteLine(e);
				return;
			}
		}
		else
		{
			Console.WriteLine("*** USING RANDOMIZED DECK OF CARDS ***\n");

			Deck myDeck = new Deck();
			Console.WriteLine("*** Shuffled 52 card deck:");
			myDeck.shuffle();
			myDeck.printDeck();
			Console.WriteLine();

			handArray = myDeck.dealHand(NUM_HANDS);

			remainingDeck = myDeck;
		}

		//Check for duplicates in test deck
		if (isTesting)
		{
			List<int> cardHashes = new List<int>();

			foreach (Hand hand in handArray)
				foreach (Card card in hand.giveSortedCardList())
				{
					int cardHash = card.getValue() * 10 + card.getSuit();

					if (cardHashes.Contains(cardHash))
					{
						Console.WriteLine("*** ERROR - DUPLICATED CARD FOUND IN DECK ***\n");

						Console.Write("*** Duplicate: ");
						card.printCard();
						Console.WriteLine(" ***");
						return;
					}
					else
						cardHashes.Add(cardHash);
				}

		}
			
		Console.WriteLine("*** Here are the six hands...");

		for (int i = 0; i < NUM_HANDS; i++)
			handArray[i].printHand();

		Console.WriteLine();

		if (!isTesting)
		{
			Console.WriteLine("*** Here is what remains in the deck...");
			remainingDeck.printDeckInOneLine();
		}
		Console.WriteLine();


		Console.WriteLine("---  WINNING HAND ORDER ---");
		foreach (Hand hand in HandAnalyzer.getRankedHands(handArray))
		{
			hand.printHandWithoutLine();
			Console.WriteLine(" - " + HandAnalyzer.handMap[HandAnalyzer.detectHandType(hand)]);
		}
	}

	public static Hand convertStringToHand(string cards)
	{
		string[] cardStrings = cards.Split(",");

		Hand hand = new Hand();

		// cardStrings look something like this: ["10H", " AS", " 3C", " 7D", " KC"]
		for (int i = 0; i < cardStrings.Length; i++)
		{
			string cardString = cardStrings[i].Trim();

			string suitString = cardString[cardString.Length -1].ToString();

			string valueString = cardString.Substring(0, cardString.Length - 1);

			Card card = new Card(valueString, suitString);
			hand.AddCard(card);
		}

		return hand;
	}

}
}
