import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class Deck
{
	private final int NUM_CARDS = 52;
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
		for (Card card: cardList)
		{
			card.printCard();
			System.out.print("  ");
		}	 

		System.out.println();
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

	public Hand dealHand()
	{
		Hand hand = new Hand();
		for (int i = 0; i < Hand.NUM_CARDS_IN_HAND; i++)
			hand.addCard(cardList.remove(0));

		return hand;
	}
}
