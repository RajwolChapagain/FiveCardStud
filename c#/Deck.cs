/*import java.util.ArrayList;
import java.util.List;
import java.util.Random;
*/
using System;
using System.Collections.Generic;

namespace FiveCardStud
{
public class Deck
{
	private const int NUM_CARDS = 52;
	private List<Card> cardList = new ArrayList<Card>();
	
	public Deck()
	{
		initializeDeck();	
	}

	private void initializeDeck()
	{
		for (int i = 0; i < NUM_CARDS; i++)
			cardList.add(new Card(i % 13, i / 13));
	}


	public void printDeck()
	{
		int i = 1;
		foreach (Card card in cardList)
		{
			card.printCard();
			Console.Write("  ");
			if (i != 0 && i % 13 == 0)
				Console.WriteLine();
			i++;
		}	 

		Console.WriteLine();
	}

	public void shuffle()
	{
		Random random = new Random();

		for (int i = 0; i < cardList.size(); i++)
		{
			Card temp = cardList.get(i);
			int swapIndex = random.nextInt(52);
			cardList.set(i, cardList.get(swapIndex));
			cardList.set(swapIndex, temp);
		}
	}

	public Hand[] dealHand(int numHands)
	{
		Hand[] handArray = new Hand[numHands];

		for (int i = 0; i < numHands; i++)
			handArray[i] = new Hand();

		for (int i = 0; i < Hand.NUM_CARDS_IN_HAND; i++)
			for (int j = 0; j < numHands; j++)
				handArray[j].addCard(cardList.remove(0));

		return handArray;
	}
}
}
