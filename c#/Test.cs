import java.util.Scanner;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.List;
import java.util.ArrayList;

public class Test
{
	public static void main(String[] args)
	{
		Hand myHand = new Hand();
		myHand.addCard(new Card("J", "H"));
		myHand.addCard(new Card("A", "C"));
		myHand.addCard(new Card("10", "S"));
		myHand.addCard(new Card("3", "H"));
		myHand.addCard(new Card("6", "S"));

		System.out.println("Pre sort:");
		myHand.printHand();
		System.out.println();

		System.out.println("Sorted array:");
		List<Card> sortedCardList = myHand.giveSortedCardList();

		for (Card card: sortedCardList)
		{
			card.printCard();
			System.out.print(" ");
		}
		System.out.println("\n");


		System.out.println("Post sort:");
		myHand.printHand();
	}
}
