import java.util.List;
import java.util.ArrayList;

public class Main
{
	public static void main(String[] args)
	{
		int NUM_HANDS = 6;
		boolean isTesting = args.length != 0;
		Hand[] handArray = new Hand[NUM_HANDS];
		Deck remainingDeck = new Deck();

		System.out.println("*** POKER HAND ANALYZER ***\n");

		if (isTesting)
		{

		}
		else
		{
			System.out.println("*** USING RANDOMIZED DECK OF CARDS ***\n");

			Deck myDeck = new Deck();
			System.out.println("*** Shuffled 52 card deck:");
			myDeck.shuffle();
			myDeck.printDeck();
			System.out.println();

			for (int i = 0; i < NUM_HANDS; i++)
				handArray[i] = myDeck.dealHand();

			remainingDeck = myDeck;
		}
			
		System.out.println("*** Here are the six hands...");

		for (int i = 0; i < NUM_HANDS; i++)
			handArray[i].printHand();

		System.out.println();


		if (!isTesting)
		{
			System.out.println("*** Here is what remains in the deck...");
			remainingDeck.printDeck();
		}
	}

}
