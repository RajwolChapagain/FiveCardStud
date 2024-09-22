using System.Collections.Generic;
using System;
using System.IO;

using FiveCardStud;

namespace FiveCardStud
{
public class Hand : IComparable<Hand>
{
	public const int NUM_CARDS_IN_HAND = 5;
	public List<Card> cardList = new List<Card>();
	private int relativeStrength = -1;

	public Hand()
	{
		
	}


	public void AddCard(Card card)
	{	
		if (cardList.size() < NUM_CARDS_IN_HAND)
			cardList.add(card);
		else
			Console.WriteLine("Error: Hand is full");
	}

	public void printHand()
	{
		foreach (Card card in cardList)
		{
			card.printCard();
			Console.Write("  ");
		}

		Console.WriteLine();
	}

	public void printHandWithoutLine()
	{
		foreach (Card card in cardList)
		{
			card.printCard();
			Console.Write("  ");
		}
	}

	public List<Card> giveSortedCardList()
	{
		List<Card> newCardArray = new List<>(cardList);
		Collections.sort(newCardArray);
		return newCardArray;
	}

	public int getRelativeStrength()
	{
		return relativeStrength;
	}

	public void setRelativeStrength(int value)
	{
		relativeStrength = value;
	}

	//Returns 1 if other hand is stronger than this hand in contrast to regular compareTo methods
	public int compareTo(Hand other)
	{
		return Integer.compare(other.relativeStrength, relativeStrength);
	}
}
}
