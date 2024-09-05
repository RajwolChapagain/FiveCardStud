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
}
