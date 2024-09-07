import java.util.Random;

public class Deck
{
	private Card[] cardArray = new Card[52];
	
	public Deck()
	{
		initializeDeck();	
	}

	private void initializeDeck()
	{
		for (int i = 0; i < cardArray.length; i++)
			cardArray[i] = new Card(i % 13, i / 13);
	}


	public void printDeck()
	{
		for (Card card: cardArray)
		{
			card.printCard();
			System.out.print("  ");
		}	 

		System.out.println();
	}

	public void shuffle()
	{
		Random random = new Random();

		for (int i = 0; i < cardArray.length; i++)
		{
			Card temp = cardArray[i];
			int swapIndex = random.nextInt(52);
			cardArray[i] = cardArray[swapIndex];
			cardArray[swapIndex] = temp;
		}
	}
}
