public class HandAnalyzer
{
	private static final String[] handMap = { "Royal Straight Flush", "Straight Flush", "Four of a Kind", "Full House", "Flush", "Straight", "Three of a Kind", "Two Pair", "Pair", "High Card" };
	private static enum handType { ROYAL_STRAIGHT_FLUSH, STRAIGHT_FLUSH, FOUR_OF_A_KIND, FULL_HOUSE, FLUSH, STRAIGHT, THREE_OF_A_KIND, TWO_PAIR, PAIR, HIGH_CARD };

	private static boolean isStraight(Hand hand)
	{
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
