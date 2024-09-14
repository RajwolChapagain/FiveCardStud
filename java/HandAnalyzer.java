import java.util.List;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Collections;
import java.util.*;

public class HandAnalyzer
{
	public static final String[] handMap = { "High Card", "Pair", "Two Pair", "Three of a Kind", "Straight", "Flush", "Full House", "Four of a Kind", "Straight Flush", "Royal Straight Flush" };

	private static enum handType { HIGH_CARD, PAIR, TWO_PAIR, THREE_OF_A_KIND, STRAIGHT, FLUSH, FULL_HOUSE, FOUR_OF_A_KIND, STRAIGHT_FLUSH, ROYAL_STRAIGHT_FLUSH };

	public static int detectHandType(Hand hand)
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

	public static Hand[] getRankedHands(Hand[] hands)
	{
		Hand[] rankedHands = Arrays.copyOf(hands, hands.length);

		//Rank by hand type
		for (Hand hand: rankedHands)
			hand.setRelativeStrength(detectHandType(hand));

		Arrays.sort(rankedHands);
		
		List<Integer> handStrengths = new ArrayList<Integer>();
		
		for (Hand hand: rankedHands)
			handStrengths.add(hand.getRelativeStrength());

		List<Integer> unrankedSubArrays = new ArrayList<>();
		
		boolean countingStarted = false;
		int countingValue = -1;
		
		for (int i = 0; i < handStrengths.size(); i++)
		{
			if (Collections.frequency(handStrengths, handStrengths.get(i)) > 1)
			{
				if (!countingStarted)
				{
					countingStarted = true;
					unrankedSubArrays.add(i);
					countingValue = handStrengths.get(i);
				}
				else
				{
					if (handStrengths.get(i) != countingValue)
					{
						unrankedSubArrays.add(i-1);	
						unrankedSubArrays.add(i);
						countingValue = handStrengths.get(i);
					}
					else
					{
						if  (i == handStrengths.size() - 1)
							unrankedSubArrays.add(i);
					}
				}
			}
			else
			{
				if (countingStarted)
				{
					unrankedSubArrays.add(i-1);
					countingStarted = false;
				}
			}

		}

		boolean skip = false;

		for (int i = 0; i < unrankedSubArrays.size(); i++)
		{
			if (!skip)
				sortSubArray(rankedHands, unrankedSubArrays.get(i), unrankedSubArrays.get(i+1));

			skip = !skip;
		}
		return rankedHands;
	}

	public static void sortSubArray(Hand[] rankedHands, int startIndex, int endIndex)
	{
		Comparator<Hand> comparator = new Comparator<Hand>()
		{
			@Override
			public int compare(Hand firstHand, Hand secondHand) 
			{
				if (detectHandType(rankedHands[startIndex]) == handType.ROYAL_STRAIGHT_FLUSH.ordinal())
					return compareRoyalFlushTie(firstHand, secondHand);
				if (detectHandType(rankedHands[startIndex]) == handType.STRAIGHT_FLUSH.ordinal())
					return compareStraightFlushTie(firstHand, secondHand);
				if (detectHandType(rankedHands[startIndex]) == handType.FOUR_OF_A_KIND.ordinal())
					return compareFourOfAKindTie(firstHand, secondHand);
				if (detectHandType(rankedHands[startIndex]) == handType.FULL_HOUSE.ordinal())
					return compareFullHouseTie(firstHand, secondHand);
				if (detectHandType(rankedHands[startIndex]) == handType.FLUSH.ordinal())
					return compareFlushTie(firstHand, secondHand);
				if (detectHandType(rankedHands[startIndex]) == handType.STRAIGHT.ordinal())
					return compareStraightTie(firstHand, secondHand);
				if (detectHandType(rankedHands[startIndex]) == handType.THREE_OF_A_KIND.ordinal())
					return compareThreeOfAKindTie(firstHand, secondHand);
				if (detectHandType(rankedHands[startIndex]) == handType.TWO_PAIR.ordinal())
					return compareTwoPairTie(firstHand, secondHand);
				if (detectHandType(rankedHands[startIndex]) == handType.PAIR.ordinal())
					return comparePairTie(firstHand, secondHand);
				if (detectHandType(rankedHands[startIndex]) == handType.HIGH_CARD.ordinal())
					return compareHighCardTie(firstHand, secondHand);
				return compareHighCardTie(firstHand, secondHand);
			}

		};

		List<Hand> subArray = new ArrayList<Hand>();

		for (int i = startIndex; i < endIndex + 1; i++)
			subArray.add(rankedHands[i]);

		Collections.sort(subArray, comparator);

		for (int i = 0; i < subArray.size(); i++)
			rankedHands[startIndex + i] = subArray.get(i);
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


	//Returns either 1 if secondHand is stronger
	//or -1 if firstHand is stronger
	public static int compareFourOfAKindTie(Hand firstHand, Hand secondHand)
	{	
		int firstHandMatchingCardValue = getMatchingCardValueInFourOfAKind(firstHand);
		int secondHandMatchingCardValue = getMatchingCardValueInFourOfAKind(secondHand);

		//This is to treat the ace as high
		if (firstHandMatchingCardValue == 0)
			firstHandMatchingCardValue = 13;

		if (secondHandMatchingCardValue == 0)
			secondHandMatchingCardValue = 13;
			
		if (firstHandMatchingCardValue > secondHandMatchingCardValue)
			return -1;

		return 1;
	}
	
	//For a Four of a Kind hand, returns the card value of the repeated card
	public static int getMatchingCardValueInFourOfAKind(Hand hand)
	{
		List<Card> sortedCardList = hand.giveSortedCardList();

		int prevValue = sortedCardList.get(0).getValue();

		for (int i = 1; i < sortedCardList.size(); i++)
		{
			int currentValue = sortedCardList.get(i).getValue();
			
			if (currentValue == prevValue)
				return currentValue;
			else
				prevValue = currentValue;
		}

		return -1;
	}

	//Returns either 1 if secondHand is stronger
	//or -1 if firstHand is stronger
	public static int compareThreeOfAKindTie(Hand firstHand, Hand secondHand)
	{	
		int firstHandMatchingCardValue = getMatchingCardValueInThreeOfAKind(firstHand);
		int secondHandMatchingCardValue = getMatchingCardValueInThreeOfAKind(secondHand);

		//This is to treat the ace as high
		if (firstHandMatchingCardValue == 0)
			firstHandMatchingCardValue = 13;

		if (secondHandMatchingCardValue == 0)
			secondHandMatchingCardValue = 13;
			
		if (firstHandMatchingCardValue > secondHandMatchingCardValue)
			return -1;

		return 1;
	}
	
	//For a Three of a Kind hand, returns the card value of the repeated card
	public static int getMatchingCardValueInThreeOfAKind(Hand hand)
	{
		List<Card> sortedCardList = hand.giveSortedCardList();

		int prevValue = sortedCardList.get(0).getValue();
		int sameCounter = 1;

		for (int i = 1; i < sortedCardList.size(); i++)
		{
			int currentValue = sortedCardList.get(i).getValue();
			
			if (currentValue == prevValue)
			{
				sameCounter += 1;
				
				if (sameCounter == 3)
					return currentValue;
			}
			else
			{
				sameCounter = 1;
				prevValue = currentValue;
			}
		}

		return -1;
	}

	//Returns either 1 if secondHand is stronger
	//or -1 if firstHand is stronger
	public static int compareFullHouseTie(Hand firstHand, Hand secondHand)
	{	
		int firstHandMatchingCardValue = getMatchingCardValueInThreeOfAKind(firstHand);
		int secondHandMatchingCardValue = getMatchingCardValueInThreeOfAKind(secondHand);

		//This is to treat the ace as high
		if (firstHandMatchingCardValue == 0)
			firstHandMatchingCardValue = 13;

		if (secondHandMatchingCardValue == 0)
			secondHandMatchingCardValue = 13;
			
		if (firstHandMatchingCardValue > secondHandMatchingCardValue)
			return -1;

		return 1;
	}

	//Returns 1 if secondHand is stronger
	//Returns -1 if firstHand is stronger
	public static int compareRoyalFlushTie(Hand firstHand, Hand secondHand)
	{
		int firstSuit = firstHand.giveSortedCardList().get(0).getSuit();
		int secondSuit = secondHand.giveSortedCardList().get(0).getSuit();

		if (firstSuit > secondSuit)
			return -1;

		return 1;
	}

	public static int compareStraightFlushTie(Hand firstHand, Hand secondHand)
	{
		int highestCardComparison = compareHighestCard(firstHand, secondHand);

		if (highestCardComparison != 0)
			return highestCardComparison;

		//Enforce suit-tie breaking
		//OPTIMIZATION: Don't need to find the suit of highest card,
		//just any card will do if it's a flush
		return compareSuitOfHighestCard(firstHand, secondHand);
	}

	public static int compareFlushTie(Hand firstHand, Hand secondHand)
	{
		int highestCardComparison = compareHighestCard(firstHand, secondHand);

		if (highestCardComparison != 0)
			return highestCardComparison;

		//Enforce suit-tie breaking
		//OPTIMIZATION: Don't need to find the suit of highest card,
		//just any card will do for flush
		return compareSuitOfHighestCard(firstHand, secondHand);
	}

	public static int compareStraightTie(Hand firstHand, Hand secondHand)
	{
		int highestCardComparison = compareHighestCard(firstHand, secondHand);

		if (highestCardComparison != 0)
			return highestCardComparison;

		//Enforce suit-tie breaking
		return compareSuitOfHighestCard(firstHand, secondHand);
	}

	public static int compareTwoPairTie(Hand firstHand, Hand secondHand)
	{
		List<Card> cardList1 = firstHand.giveSortedCardList();
		List<Card> cardList2 = secondHand.giveSortedCardList();

		List<Integer> valueList1 = new ArrayList<Integer>();
		List<Integer> valueList2 = new ArrayList<Integer>();

		for (int i = 0; i < cardList1.size(); i++)
		{
			if (cardList1.get(i).getValue() == 0)
				valueList1.add(13);
			else
				valueList1.add(cardList1.get(i).getValue());

			if (cardList2.get(i).getValue() == 0)
				valueList2.add(13);
			else
				valueList2.add(cardList2.get(i).getValue());
		}

		Collections.sort(valueList1, Collections.reverseOrder());
		Collections.sort(valueList2, Collections.reverseOrder());

		List<Integer> pairCardList1 = new ArrayList<Integer>();
		List<Integer> pairCardList2 = new ArrayList<Integer>();
		

		int prevValue = valueList1.get(0);

		for (int i = 1; i < valueList1.size(); i++)
		{
			int currentValue = valueList1.get(i);

			if (currentValue == prevValue)
				pairCardList1.add(currentValue);
			else
				prevValue = currentValue;
		}

		prevValue = valueList2.get(0);

		for (int i = 1; i < valueList2.size(); i++)
		{
			int currentValue = valueList2.get(i);

			if (currentValue == prevValue)
				pairCardList2.add(currentValue);
			else
				prevValue = currentValue;
		}

		//Compare highest pair
		if (Collections.max(pairCardList1) > Collections.max(pairCardList2))
			return -1;
		else if (Collections.max(pairCardList1) < Collections.max(pairCardList2))
			return 1;
		
		// If the highest pair is tied, compare lowest pair
		if (Collections.min(pairCardList1) > Collections.min(pairCardList2))
			return -1;
		else if (Collections.min(pairCardList1) < Collections.min(pairCardList2))
			return 1;

		//If both highest and lowest pair are tied, compare kicker value
		int kickerValue1 = -1;
		int kickerValue2 = -1;

		for (int value: valueList1)
		{
			if (Collections.frequency(valueList1, value) == 1)
			{
				kickerValue1 = value;
				break;
			}
		}

		for (int value: valueList2)
		{
			if (Collections.frequency(valueList2, value) == 1)
			{
				kickerValue2 = value;
				break;
			}
		}

		if (kickerValue1 > kickerValue2)
			return -1;
		else if (kickerValue1 < kickerValue2)
			return 1;

		//If both kicker values are equal, compare kicker suit
		
		int kickerSuit1 = -1;
		int kickerSuit2 = -1;

		for (int i = 0; i < cardList1.size(); i++)
		{
			if (cardList1.get(i).getValue() == kickerValue1)
			{
				kickerSuit1 = cardList1.get(i).getSuit();
				break;
			}
		}

		for (int i = 0; i < cardList2.size(); i++)
		{
			if (cardList2.get(i).getValue() == kickerValue2)
			{
				kickerSuit2 = cardList2.get(i).getSuit();
				break;
			}
		}

		if (kickerSuit1 > kickerSuit2)
			return -1;

		return 1;
	}

	public static int comparePairTie(Hand firstHand, Hand secondHand)
	{
		List<Card> cardList1 = firstHand.giveSortedCardList();
		List<Card> cardList2 = secondHand.giveSortedCardList();

		List<Integer> valueList1 = new ArrayList<Integer>();
		List<Integer> valueList2 = new ArrayList<Integer>();

		for (int i = 0; i < cardList1.size(); i++)
		{
			if (cardList1.get(i).getValue() == 0)
				valueList1.add(13);
			else
				valueList1.add(cardList1.get(i).getValue());

			if (cardList2.get(i).getValue() == 0)
				valueList2.add(13);
			else
				valueList2.add(cardList2.get(i).getValue());
		}

		Collections.sort(valueList1, Collections.reverseOrder());
		Collections.sort(valueList2, Collections.reverseOrder());

		List<Integer> pairCardList1 = new ArrayList<Integer>();
		List<Integer> pairCardList2 = new ArrayList<Integer>();
		

		int prevValue = valueList1.get(0);

		for (int i = 1; i < valueList1.size(); i++)
		{
			int currentValue = valueList1.get(i);

			if (currentValue == prevValue)
				pairCardList1.add(currentValue);
			else
				prevValue = currentValue;
		}

		prevValue = valueList2.get(0);

		for (int i = 1; i < valueList2.size(); i++)
		{
			int currentValue = valueList2.get(i);

			if (currentValue == prevValue)
				pairCardList2.add(currentValue);
			else
				prevValue = currentValue;
		}

		//Compare highest pair
		if (Collections.max(pairCardList1) > Collections.max(pairCardList2))
			return -1;
		else if (Collections.max(pairCardList1) < Collections.max(pairCardList2))
			return 1;

		Hand shortHand1 = new Hand();
		Hand shortHand2 = new Hand();

		for (Card card: firstHand.cardList)
		{
			int cardValue = card.getValue();
			if (cardValue == 0)
				cardValue = 13;

			if (!pairCardList1.contains(cardValue))
				shortHand1.addCard(card);
		}

		for (Card card: secondHand.cardList)
		{
			int cardValue = card.getValue();
			if (cardValue == 0)
				cardValue = 13;

			if (!pairCardList2.contains(cardValue))
				shortHand2.addCard(card);
		}

		int highestCardComparison = compareHighestCard(shortHand1, shortHand2);

		if (highestCardComparison != 0)
			return highestCardComparison;

		return compareSuitOfHighestCard(shortHand1, shortHand2);
	}

	public static int compareHighCardTie(Hand firstHand, Hand secondHand)
	{
		int highestCardComparison = compareHighestCard(firstHand, secondHand);

		if (highestCardComparison != 0)
			return highestCardComparison;

		//Enforce suit-tie breaking
		return compareSuitOfHighestCard(firstHand, secondHand);
	}

	public static int compareSuitOfHighestCard(Hand firstHand, Hand secondHand)
	{
		List<Card> cardList1 = firstHand.giveSortedCardList();
		List<Card> cardList2 = secondHand.giveSortedCardList();

		List<Integer> valueList1 = new ArrayList<Integer>();
		List<Integer> valueList2 = new ArrayList<Integer>();

		for (int i = 0; i < cardList1.size(); i++)
		{
			if (cardList1.get(i).getValue() == 0)
			{
				//Account for when Aces are low in a straight or straight flush
				if (detectHandType(firstHand) != handType.STRAIGHT_FLUSH.ordinal() && 
						detectHandType(firstHand) != handType.STRAIGHT.ordinal())
					valueList1.add(13);
				else
					valueList1.add(0);
			}
			else
				valueList1.add(cardList1.get(i).getValue());

			if (cardList2.get(i).getValue() == 0)
			{
				if (detectHandType(secondHand) != handType.STRAIGHT_FLUSH.ordinal() && 
						detectHandType(secondHand) != handType.STRAIGHT.ordinal())
					valueList2.add(13);
				else
					valueList2.add(0);
			}
			else
				valueList2.add(cardList2.get(i).getValue());
		}

		int maxValue1 = Collections.max(valueList1);
		int maxValue1Index = valueList1.indexOf(maxValue1);

		int maxValue2 = Collections.max(valueList2);
		int maxValue2Index = valueList2.indexOf(maxValue2);

		int suitOfHighestCard1 = cardList1.get(maxValue1Index).getSuit();
		int suitOfHighestCard2 = cardList2.get(maxValue2Index).getSuit();
		
		if (suitOfHighestCard1 > suitOfHighestCard2)
			return -1;
	
		return 1;
	}

	//Used to compare: Straight Flushes, Flushes, Straights, and High Card
	//Returns:
	//1 if secondHand is stronger
	//0 if both are tied
	//-1 if firstHand is stronger
	public static int compareHighestCard(Hand firstHand, Hand secondHand)
	{
		List<Card> cardList1 = firstHand.giveSortedCardList();
		List<Card> cardList2 = secondHand.giveSortedCardList();

		List<Integer> valueList1 = new ArrayList<Integer>();
		List<Integer> valueList2 = new ArrayList<Integer>();

		for (int i = 0; i < cardList1.size(); i++)
		{
			if (cardList1.get(i).getValue() == 0)
			{
				//Account for when Aces are low in a straight or straight flush
				if (detectHandType(firstHand) != handType.STRAIGHT_FLUSH.ordinal() && 
						detectHandType(firstHand) != handType.STRAIGHT.ordinal())
					valueList1.add(13);
				else
					valueList1.add(0);
			}
			else
				valueList1.add(cardList1.get(i).getValue());

			if (cardList2.get(i).getValue() == 0)
			{
				if (detectHandType(secondHand) != handType.STRAIGHT_FLUSH.ordinal() && 
						detectHandType(secondHand) != handType.STRAIGHT.ordinal())
					valueList2.add(13);
				else
					valueList2.add(0);
			}
			else
				valueList2.add(cardList2.get(i).getValue());
		}

		Collections.sort(valueList1, Collections.reverseOrder());
		Collections.sort(valueList2, Collections.reverseOrder());
		
		for (int i = 0; i < valueList1.size(); i++)
		{
			if (valueList1.get(i) > valueList2.get(i))
				return -1;
			else if (valueList1.get(i) < valueList2.get(i))
				return 1;
		}

		return 0;
	}
}
