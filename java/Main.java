
public class Main
{
	public static void main(String[] args)
	{
		System.out.println("*** POKER HAND ANALYZER ***\n");

		System.out.println("*** USING RANDOMIZED DECK OF CARDS ***\n");

		Deck myDeck = new Deck();
		System.out.println("*** Shuffled 52 card deck:");
		myDeck.shuffle();
		myDeck.printDeck();
		System.out.println();
			
		System.out.println("*** Here are the six hands...");

		for (int i = 0; i < 6; i++)
		{
			Hand hand = myDeck.dealHand();
			hand.printHand();
		}
		System.out.println();

		System.out.println("*** Here is what remains in the deck...");
		myDeck.printDeck();
	}

}
