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
		if (cardList.Count < NUM_CARDS_IN_HAND)
			cardList.Add(card);
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
		List<Card> newCardArray = new List<Card>(cardList);
		newCardArray.Sort();
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
	public int CompareTo(Hand other)
	{
		return other.relativeStrength.CompareTo(relativeStrength);
	}
}
}
