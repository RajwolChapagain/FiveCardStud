import java.util.List;
import java.util.ArrayList;

public class HandAnalyzer
{
	public static final String[] handMap = { "High Card", "Pair", "Two Pair", "Three of a Kind", "Stright", "Flush", "Full House", "Four of a Kind", "Straight Flush", "Royal Straight Flush" };

	private static enum handType { HIGH_CARD, PAIR, TWO_PAIR, THREE_OF_A_KIND, STRAIGHT, FLUSH, FULL_HOUSE, FOUR_OF_A_KIND, STRAIGHT_FLUSH, ROYAL_STRAIGHT_FLUSH };

	public static int detectHandType (Hand hand)
	{
		if (isRoyalStraightFlush(hand))
			return handType.ROYAL_STRAIGHT_FLUSH.ordinal(); 

		if (isStraightFlush(hand))
			return handType.STRAIGHT_FLUSH.ordinal();

		if (isFourOfAKind(hand))
			return handType.FOUR_OF_A_KIND.ordinal();

		if (isFullHouse(hand))
			return handType.FULL_HOUSE.ordinal();

		if (hasFlush(hand))
			return handType.FLUSH.ordinal();

		if (hasStraight(hand))
			return handType.STRAIGHT.ordinal();

		if (hasThreeOfAKind(hand))
			return handType.THREE_OF_A_KIND.ordinal();

		if (hasTwoPair(hand))
			return handType.TWO_PAIR.ordinal();

		if (hasPair(hand))
			return handType.PAIR.ordinal();

		return handType.HIGH_CARD.ordinal();
	}

	public static boolean isRoyalStraightFlush(Hand hand)
	{	
		if (hasRoyalStraight(hand) & hasFlush(hand))
			return true;
	
		return false;
	}

	public static boolean isStraightFlush(Hand hand)
	{
		// This will also return true for Royal Straight Flush because
		// hasStraight returns true even if the hand is a Royal Straight
		if (hasStraight(hand) & hasFlush(hand))
			return true;

		return false;
	}

	public static boolean isFourOfAKind(Hand hand)
	{
		//Counts the number of same cards 
		int sameCounter = 1;

		List<Card> sortedCardList = hand.giveSortedCardList();
		int prevValue = sortedCardList.get(0).getValue();

		for (int i = 1; i < sortedCardList.size(); i++)
		{
			int currentValue = sortedCardList.get(i).getValue();
			if (currentValue == prevValue)
			{
				sameCounter += 1;

				if (sameCounter == 4)
					return true;
			}
			else
			{
				sameCounter = 1;
				prevValue = currentValue;
			}
		}
		
		return false;
	}

	//Also triggers for a Four of A Kind.
	//Hence, always check this after checking for Four of a Kind
	public static boolean isFullHouse(Hand hand)
	{
		if (!hasThreeOfAKind(hand))
			return false;

		boolean threeOfAKindDetected = false;
		boolean pairDetected = false;

		int sameCounter = 1;

		List<Card> sortedCardList = hand.giveSortedCardList();
		int prevValue = sortedCardList.get(0).getValue();

		for (int i = 1; i < sortedCardList.size(); i++)
		{
			int currentValue = sortedCardList.get(i).getValue();
			if (currentValue == prevValue)
			{
				sameCounter += 1;

				if (sameCounter == 3)
				{
					threeOfAKindDetected = true;
					sameCounter = 1;
				}
				
				if (threeOfAKindDetected & sameCounter == 2)
					pairDetected = true;
			}
			else
			{
				if (sameCounter == 2)
					pairDetected = true;

				sameCounter = 1;
				prevValue = currentValue;
			}
		}
		
		if (threeOfAKindDetected & pairDetected)
			return true;

		return false;
	}

	public static boolean hasStraight(Hand hand)
	{
		if (hasRoyalStraight(hand))
			return true;

		List<Card> sortedCardList = hand.giveSortedCardList();

		int prevValue = sortedCardList.get(0).getValue();

		for (int i = 1; i < sortedCardList.size(); i++)
		{
			int currentValue = sortedCardList.get(i).getValue();
			if (prevValue + 1 != currentValue)
				return false;
			prevValue = currentValue;
		}

		return true;	
	}

	//hasThreeOfAKind will also return true if the card is Four of a Kind;
	//it just stops counting at 3
	public static boolean hasThreeOfAKind(Hand hand)
	{
		//Counts the number of same cards 
		int sameCounter = 1;

		List<Card> sortedCardList = hand.giveSortedCardList();
		int prevValue = sortedCardList.get(0).getValue();

		for (int i = 1; i < sortedCardList.size(); i++)
		{
			int currentValue = sortedCardList.get(i).getValue();
			if (currentValue == prevValue)
			{
				sameCounter += 1;

				if (sameCounter == 3)
					return true;
			}
			else
			{
				sameCounter = 1;
				prevValue = currentValue;
			}
		}
		
		return false;
	}

	//This will easily also trigger at a Three of A Kind,
	//Four of a Kind, as well as a Full House. Make sure to 
	//check those first!
	public static boolean hasTwoPair(Hand hand)
	{
		if (!hasPair(hand))
			return false;

		boolean firstPairDetected = false;
		List<Card> sortedCardList = hand.giveSortedCardList();
		int prevValue = sortedCardList.get(0).getValue();

		for (int i = 1; i < sortedCardList.size(); i++)
		{
			int currentValue = sortedCardList.get(i).getValue();
			if (currentValue == prevValue)
			{
				if (!firstPairDetected)
					firstPairDetected = true;
				else
					return true;
			}
			else
				prevValue = currentValue;
		}

		return false;
	}

	//hasPair will also return true if the card has Two Pairs, is Three or Four of a Kind;
	//it just stops looking when it finds a pair
	public static boolean hasPair(Hand hand)
	{
		List<Card> sortedCardList = hand.giveSortedCardList();
		int prevValue = sortedCardList.get(0).getValue();

		for (int i = 1; i < sortedCardList.size(); i++)
		{
			int currentValue = sortedCardList.get(i).getValue();
			if (currentValue == prevValue)
				return true;
			else
				prevValue = currentValue;
		}
		
		return false;
	}

	public static boolean hasRoyalStraight(Hand hand)
	{
		List<Card> sortedCardList = hand.giveSortedCardList();

		if (sortedCardList.get(0).getValue() == 0 & sortedCardList.get(1).getValue() == 9 &
				sortedCardList.get(2).getValue() == 10 & sortedCardList.get(3).getValue()
				== 11 & sortedCardList.get(4).getValue() == 12)
			return true;
	
		return false;
	}

	public static boolean hasFlush(Hand hand)
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
