import java.util.List;
import java.util.ArrayList;

public class HandAnalyzer
{
	private static final String[] handMap = { "High Card", "Pair", "Two Pair", "Three of a Kind", "Stright", "Flush", "Full House", "Four of a Kind", "Straight Flush", "Royal Straight Flush" };

	private static enum handType { HIGH_CARD, PAIR, TWO_PAIR, THREE_OF_A_KIND, STRAIGHT, FLUSH, FULL_HOUSE, FOUR_OF_A_KIND, STRAIGHT_FLUSH, ROYAL_STRAIGHT_FLUSH };

	public static boolean isRoyalStraightFlush(Hand hand)
	{	
		if (isRoyalStraight(hand) & isFlush(hand))
			return true;
	
		return false;
	}

	public static boolean isStraight(Hand hand)
	{
		if (isRoyalStraight(hand))
			return true;

		return false;	
	}

	public static boolean isRoyalStraight(Hand hand)
	{
		List<Card> sortedCardList = hand.giveSortedCardList();

		if (sortedCardList.get(0).getValue() == 0 & sortedCardList.get(1).getValue() == 9 &
				sortedCardList.get(2).getValue() == 10 & sortedCardList.get(3).getValue()
				== 11 & sortedCardList.get(4).getValue() == 12)
			return true;
	
		return false;
	}

	public static boolean isFlush(Hand hand)
	{
		int prevSuit = hand.giveSortedCardList().get(0).getSuit();

		for (Card card : hand.giveSortedCardList())
		{
			if (card.getSuit() != prevSuit)
				return false;
			prevSuit = card.getSuit();
		}

		return true;
	}
}
