import java.util.List;
import java.util.ArrayList;
import java.util.Collections;

public class Hand implements Comparable<Hand>
{
	public static final int NUM_CARDS_IN_HAND = 5;
	public List<Card> cardList = new ArrayList<Card>();
	private int relativeStrength = -1;

	public Hand()
	{
		
	}


	public void addCard(Card card)
	{	
		if (cardList.size() < NUM_CARDS_IN_HAND)
			cardList.add(card);
		else
			System.out.println("Error: Hand is full");
	}

	public void printHand()
	{
		for (Card card: cardList)
		{
			card.printCard();
			System.out.print("  ");
		}

		System.out.println();
	}

	public void printHandWithoutLine()
	{
		for (Card card: cardList)
		{
			card.printCard();
			System.out.print("  ");
		}
	}

	public List<Card> giveSortedCardList()
	{
		List<Card> newCardArray = new ArrayList<>(cardList);
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
	@Override
	public int compareTo(Hand other)
	{
		return Integer.compare(other.relativeStrength, relativeStrength);
	}
}

