import java.util.List;
import java.util.ArrayList;

public class Hand
{
	public static final int NUM_CARDS_IN_HAND = 5;
	private List<Card> cardList = new ArrayList<Card>();

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
}
