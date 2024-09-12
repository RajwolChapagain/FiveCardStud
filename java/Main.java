import java.util.List;
import java.util.ArrayList;
import java.util.Scanner;
import java.io.File;
import java.io.FileNotFoundException;

public class Main
{
	public static void main(String[] args)
	{
		int NUM_HANDS = 6;
		boolean isTesting = args.length != 0;
		Hand[] handArray = new Hand[NUM_HANDS];
		Deck remainingDeck;

		System.out.println("*** POKER HAND ANALYZER ***\n");

		if (isTesting)
		{
			remainingDeck = new Deck();
			System.out.println("*** USING TEST DECK ***\n");
			
			String relativeFilePath = args[0];
			System.out.println("*** File: " + relativeFilePath);
			try
			{
				Scanner scanner = new Scanner(new File(relativeFilePath));
				int handNumber = 0;
				while (scanner.hasNextLine())
				{
					String line = scanner.nextLine();
					System.out.println(line);
					handArray[handNumber] = convertStringToHand(line);
					handNumber += 1;
				}
				System.out.println();
			}
			catch (FileNotFoundException e)
			{
				System.out.println("Error: File " + relativeFilePath + " not found");
				return;
			}
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

	public static Hand convertStringToHand(String cards)
	{
		String[] cardStrings = cards.split(",");

		Hand hand = new Hand();

		// cardStrings look something like this: ["10H", " AS", " 3C", " 7D", " KC"]
		for (int i = 0; i < cardStrings.length; i++)
		{
			String cardString = cardStrings[i].trim();

			String suitString = Character.toString(cardString.charAt(cardString.length() -1));

			String valueString = cardString.substring(0, cardString.length() - 1);

			Card card = new Card(valueString, suitString);
			hand.addCard(card);
		}

		return hand;
	}

}
