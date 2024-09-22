using System;

namespace FiveCardStud
{
public class Card : IComparable<Card>
{
	private static string[] valueMap = {"A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"};
	private static string[] suitMap = {"D", "C", "H", "S"};

	private int value;
	private int suit;

	public Card(int value, int suit)
	{
		if (value < 0 || value >= valueMap.Length || suit < 0 || suit >= suitMap.Length)
			throw new InvalidCardException("A card can't have a value index of " + value + " and/or a suit index of " + suit);
		this.value = value;
		this.suit = suit;	
	}

	public Card(string valueString, string suitString) 
		: this(getIndexOfValue(valueString), getIndexOfSuit(suitString))
	{
	}

	public int getValue()
	{
		return value;
	}

	public int getSuit()
	{
		return suit;
	}

	public void printCard()
	{
		Console.Write(valueMap[value] + suitMap[suit]);
	}

	// This function returns -1 if this card is smaller, 0 if the cards are equal, and 1 if this card is larger
	public int CompareTo(Card other)
	{
		if (this.value > other.value)
			return 1;
		else if (this.value < other.value)
			return -1;
	
		return 0;
	}

	public static int getIndexOfValue(string value)
	{
		for (int i = 0; i < valueMap.Length; i++)
			if (valueMap[i].Equals(value))
				return i;
		return -1;
	}

	public static int getIndexOfSuit(string suit)
	{
		for (int i = 0; i < suitMap.Length; i++)
			if (suitMap[i].Equals(suit))
				return i;
		return -1;
	}
}

class InvalidCardException : Exception {

	string message;
	public InvalidCardException(string message)
	{
		this.message = message;
	}
}
}
