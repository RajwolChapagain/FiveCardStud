/*import java.util.List;
import java.util.Arrays;
import java.util.List;
import java.util.Collections;
import java.util.*;
*/
using System;
using System.Collections.Generic;
using System.Linq;
using FiveCardStud;

namespace FiveCardStud 
{
public class HandAnalyzer
{
	public static String[] handMap = { "High Card", "Pair", "Two Pair", "Three of a Kind", "Straight", "Flush", "Full House", "Four of a Kind", "Straight Flush", "Royal Straight Flush" };

	private enum handType { HIGH_CARD, PAIR, TWO_PAIR, THREE_OF_A_KIND, STRAIGHT, FLUSH, FULL_HOUSE, FOUR_OF_A_KIND, STRAIGHT_FLUSH, ROYAL_STRAIGHT_FLUSH };

	public static int detectHandType(Hand hand)
	{
		if (isRoyalStraightFlush(hand))
			return (int) handType.ROYAL_STRAIGHT_FLUSH; 

		if (isStraightFlush(hand))
			return (int) handType.STRAIGHT_FLUSH;

		if (isFourOfAKind(hand))
			return (int) handType.FOUR_OF_A_KIND;

		if (isFullHouse(hand))
			return (int) handType.FULL_HOUSE;

		if (hasFlush(hand))
			return (int) handType.FLUSH;

		if (hasStraight(hand))
			return (int) handType.STRAIGHT;

		if (hasThreeOfAKind(hand))
			return (int) handType.THREE_OF_A_KIND;

		if (hasTwoPair(hand))
			return (int) handType.TWO_PAIR;

		if (hasPair(hand))
			return (int) handType.PAIR;

		return (int) handType.HIGH_CARD;
	}

	public static Hand[] getRankedHands(Hand[] hands)
	{
		Hand[] rankedHands = new Hand[hands.Length];
		Array.Copy(hands, rankedHands, hands.Length);

		//Rank by hand type
		foreach (Hand hand in rankedHands)
			hand.setRelativeStrength(detectHandType(hand));

		Array.Sort(rankedHands);
		
		List<int> handStrengths = new List<int>();
		
		foreach (Hand hand in rankedHands)
			handStrengths.Add(hand.getRelativeStrength());

		List<int> unrankedSubArrays = new List<int>();
		
		bool countingStarted = false;
		int countingValue = -1;
		
		for (int i = 0; i < handStrengths.Count; i++)
		{
			if (handStrengths.Count(x => x == handStrengths[i]) > 1)
			{
				if (!countingStarted)
				{
					countingStarted = true;
					unrankedSubArrays.Add(i);
					countingValue = handStrengths[i];
				}
				else
				{
					if (handStrengths[i] != countingValue)
					{
						unrankedSubArrays.Add(i-1);	
						unrankedSubArrays.Add(i);
						countingValue = handStrengths[i];
					}
					else
					{
						if  (i == handStrengths.Count - 1)
							unrankedSubArrays.Add(i);
					}
				}
			}
			else
			{
				if (countingStarted)
				{
					unrankedSubArrays.Add(i-1);
					countingStarted = false;
				}
			}

		}

		bool skip = false;

		for (int i = 0; i < unrankedSubArrays.Count; i++)
		{
			if (!skip)
				sortSubArray(rankedHands, unrankedSubArrays[i], unrankedSubArrays[i+1]);

			skip = !skip;
		}
		return rankedHands;
	}

	public static void sortSubArray(Hand[] rankedHands, int startIndex, int endIndex)
	{
		Comparison<Hand> comparator = (firstHand, secondHand) =>
		{
				if (detectHandType(rankedHands[startIndex]) == (int) handType.ROYAL_STRAIGHT_FLUSH)
					return compareRoyalFlushTie(firstHand, secondHand);
				if (detectHandType(rankedHands[startIndex]) == (int) handType.STRAIGHT_FLUSH)
					return compareStraightFlushTie(firstHand, secondHand);
				if (detectHandType(rankedHands[startIndex]) == (int) handType.FOUR_OF_A_KIND)
					return compareFourOfAKindTie(firstHand, secondHand);
				if (detectHandType(rankedHands[startIndex]) == (int) handType.FULL_HOUSE)
					return compareFullHouseTie(firstHand, secondHand);
				if (detectHandType(rankedHands[startIndex]) == (int) handType.FLUSH)
					return compareFlushTie(firstHand, secondHand);
				if (detectHandType(rankedHands[startIndex]) == (int) handType.STRAIGHT)
					return compareStraightTie(firstHand, secondHand);
				if (detectHandType(rankedHands[startIndex]) == (int) handType.THREE_OF_A_KIND)
					return compareThreeOfAKindTie(firstHand, secondHand);
				if (detectHandType(rankedHands[startIndex]) == (int) handType.TWO_PAIR)
					return compareTwoPairTie(firstHand, secondHand);
				if (detectHandType(rankedHands[startIndex]) == (int) handType.PAIR)
					return comparePairTie(firstHand, secondHand);
				if (detectHandType(rankedHands[startIndex]) == (int) handType.HIGH_CARD)
					return compareHighCardTie(firstHand, secondHand);
				return compareHighCardTie(firstHand, secondHand);
		};

		List<Hand> subArray = new List<Hand>();

		for (int i = startIndex; i < endIndex + 1; i++)
			subArray.Add(rankedHands[i]);

		subArray.Sort(comparator);

		for (int i = 0; i < subArray.Count; i++)
			rankedHands[startIndex + i] = subArray[i];
	}

	public static bool isRoyalStraightFlush(Hand hand)
	{	
		if (hasRoyalStraight(hand) & hasFlush(hand))
			return true;
	
		return false;
	}

	public static bool isStraightFlush(Hand hand)
	{
		// This will also return true for Royal Straight Flush because
		// hasStraight returns true even if the hand is a Royal Straight
		if (hasStraight(hand) & hasFlush(hand))
			return true;

		return false;
	}

	public static bool isFourOfAKind(Hand hand)
	{
		//Counts the number of same cards 
		int sameCounter = 1;

		List<Card> sortedCardList = hand.giveSortedCardList();
		int prevValue = sortedCardList[0].getValue();

		for (int i = 1; i < sortedCardList.Count; i++)
		{
			int currentValue = sortedCardList[i].getValue();
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
	public static bool isFullHouse(Hand hand)
	{
		if (!hasThreeOfAKind(hand))
			return false;

		bool threeOfAKindDetected = false;
		bool pairDetected = false;

		int sameCounter = 1;

		List<Card> sortedCardList = hand.giveSortedCardList();
		int prevValue = sortedCardList[0].getValue();

		for (int i = 1; i < sortedCardList.Count; i++)
		{
			int currentValue = sortedCardList[i].getValue();
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

	public static bool hasStraight(Hand hand)
	{
		if (hasRoyalStraight(hand))
			return true;

		List<Card> sortedCardList = hand.giveSortedCardList();

		int prevValue = sortedCardList[0].getValue();

		for (int i = 1; i < sortedCardList.Count; i++)
		{
			int currentValue = sortedCardList[i].getValue();
			if (prevValue + 1 != currentValue)
				return false;
			prevValue = currentValue;
		}

		return true;	
	}

	//hasThreeOfAKind will also return true if the card is Four of a Kind;
	//it just stops counting at 3
	public static bool hasThreeOfAKind(Hand hand)
	{
		//Counts the number of same cards 
		int sameCounter = 1;

		List<Card> sortedCardList = hand.giveSortedCardList();
		int prevValue = sortedCardList[0].getValue();

		for (int i = 1; i < sortedCardList.Count; i++)
		{
			int currentValue = sortedCardList[i].getValue();
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
	public static bool hasTwoPair(Hand hand)
	{
		if (!hasPair(hand))
			return false;

		bool firstPairDetected = false;
		List<Card> sortedCardList = hand.giveSortedCardList();
		int prevValue = sortedCardList[0].getValue();

		for (int i = 1; i < sortedCardList.Count; i++)
		{
			int currentValue = sortedCardList[i].getValue();
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
	public static bool hasPair(Hand hand)
	{
		List<Card> sortedCardList = hand.giveSortedCardList();
		int prevValue = sortedCardList[0].getValue();

		for (int i = 1; i < sortedCardList.Count; i++)
		{
			int currentValue = sortedCardList[i].getValue();
			if (currentValue == prevValue)
				return true;
			else
				prevValue = currentValue;
		}
		
		return false;
	}

	public static bool hasRoyalStraight(Hand hand)
	{
		List<Card> sortedCardList = hand.giveSortedCardList();

		if (sortedCardList[0].getValue() == 0 & sortedCardList[1].getValue() == 9 &
				sortedCardList[2].getValue() == 10 & sortedCardList[3].getValue()
				== 11 & sortedCardList[4].getValue() == 12)
			return true;
	
		return false;
	}

	public static bool hasFlush(Hand hand)
	{
		int prevSuit = hand.giveSortedCardList()[0].getSuit();

		foreach (Card card in hand.giveSortedCardList())
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

		int prevValue = sortedCardList[0].getValue();

		for (int i = 1; i < sortedCardList.Count; i++)
		{
			int currentValue = sortedCardList[i].getValue();
			
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

		int prevValue = sortedCardList[0].getValue();
		int sameCounter = 1;

		for (int i = 1; i < sortedCardList.Count; i++)
		{
			int currentValue = sortedCardList[i].getValue();
			
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
		int firstSuit = firstHand.giveSortedCardList()[0].getSuit();
		int secondSuit = secondHand.giveSortedCardList()[0].getSuit();

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

		List<int> valueList1 = new List<int>();
		List<int> valueList2 = new List<int>();

		for (int i = 0; i < cardList1.Count; i++)
		{
			if (cardList1[i].getValue() == 0)
				valueList1.Add(13);
			else
				valueList1.Add(cardList1[i].getValue());

			if (cardList2[i].getValue() == 0)
				valueList2.Add(13);
			else
				valueList2.Add(cardList2[i].getValue());
		}

		valueList1.Sort();
		valueList1.Reverse();
		valueList2.Sort();
		valueList2.Reverse();

		List<int> pairCardList1 = new List<int>();
		List<int> pairCardList2 = new List<int>();
		

		int prevValue = valueList1[0];

		for (int i = 1; i < valueList1.Count; i++)
		{
			int currentValue = valueList1[i];

			if (currentValue == prevValue)
				pairCardList1.Add(currentValue);
			else
				prevValue = currentValue;
		}

		prevValue = valueList2[0];

		for (int i = 1; i < valueList2.Count; i++)
		{
			int currentValue = valueList2[i];

			if (currentValue == prevValue)
				pairCardList2.Add(currentValue);
			else
				prevValue = currentValue;
		}

		//Compare highest pair
		if (pairCardList1.Max() > pairCardList2.Max())
			return -1;
		else if (pairCardList1.Max() < pairCardList2.Max())
			return 1;
		
		// If the highest pair is tied, compare lowest pair
		if (pairCardList1.Min() > pairCardList2.Min())
			return -1;
		else if (pairCardList1.Min() < pairCardList2.Min())
			return 1;

		//If both highest and lowest pair are tied, compare kicker value
		int kickerValue1 = -1;
		int kickerValue2 = -1;

		foreach (int value in valueList1)
		{
			if (valueList1.Count(x => x == value) == 1)
			{
				kickerValue1 = value;
				break;
			}
		}

		foreach (int value in valueList2)
		{
			if (valueList2.Count(x => x == value) == 1)
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

		for (int i = 0; i < cardList1.Count; i++)
		{
			if (cardList1[i].getValue() == kickerValue1)
			{
				kickerSuit1 = cardList1[i].getSuit();
				break;
			}
		}

		for (int i = 0; i < cardList2.Count; i++)
		{
			if (cardList2[i].getValue() == kickerValue2)
			{
				kickerSuit2 = cardList2[i].getSuit();
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

		List<int> valueList1 = new List<int>();
		List<int> valueList2 = new List<int>();

		for (int i = 0; i < cardList1.Count; i++)
		{
			if (cardList1[i].getValue() == 0)
				valueList1.Add(13);
			else
				valueList1.Add(cardList1[i].getValue());

			if (cardList2[i].getValue() == 0)
				valueList2.Add(13);
			else
				valueList2.Add(cardList2[i].getValue());
		}

		valueList1.Sort();
		valueList1.Reverse();
		valueList2.Sort();
		valueList2.Reverse();

		List<int> pairCardList1 = new List<int>();
		List<int> pairCardList2 = new List<int>();
		

		int prevValue = valueList1[0];

		for (int i = 1; i < valueList1.Count; i++)
		{
			int currentValue = valueList1[i];

			if (currentValue == prevValue)
				pairCardList1.Add(currentValue);
			else
				prevValue = currentValue;
		}

		prevValue = valueList2[0];

		for (int i = 1; i < valueList2.Count; i++)
		{
			int currentValue = valueList2[i];

			if (currentValue == prevValue)
				pairCardList2.Add(currentValue);
			else
				prevValue = currentValue;
		}

		//Compare highest pair
		if (pairCardList1.Max() > pairCardList2.Max())
			return -1;
		else if (pairCardList1.Max() < pairCardList2.Max())
			return 1;

		Hand shortHand1 = new Hand();
		Hand shortHand2 = new Hand();

		foreach (Card card in firstHand.cardList)
		{
			int cardValue = card.getValue();
			if (cardValue == 0)
				cardValue = 13;

			if (!pairCardList1.Contains(cardValue))
				shortHand1.AddCard(card);
		}

		foreach (Card card in secondHand.cardList)
		{
			int cardValue = card.getValue();
			if (cardValue == 0)
				cardValue = 13;

			if (!pairCardList2.Contains(cardValue))
				shortHand2.AddCard(card);
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

		List<int> valueList1 = new List<int>();
		List<int> valueList2 = new List<int>();

		for (int i = 0; i < cardList1.Count; i++)
		{
			if (cardList1[i].getValue() == 0)
			{
				//Account for when Aces are low in a straight or straight flush
				if (detectHandType(firstHand) != (int) handType.STRAIGHT_FLUSH && 
						detectHandType(firstHand) != (int) handType.STRAIGHT)
					valueList1.Add(13);
				else
					valueList1.Add(0);
			}
			else
				valueList1.Add(cardList1[i].getValue());

			if (cardList2[i].getValue() == 0)
			{
				if (detectHandType(secondHand) != (int) handType.STRAIGHT_FLUSH && 
						detectHandType(secondHand) != (int) handType.STRAIGHT)
					valueList2.Add(13);
				else
					valueList2.Add(0);
			}
			else
				valueList2.Add(cardList2[i].getValue());
		}

		int maxValue1 = valueList1.Max();
		int maxValue1Index = valueList1.IndexOf(maxValue1);

		int maxValue2 = valueList2.Max();
		int maxValue2Index = valueList2.IndexOf(maxValue2);

		int suitOfHighestCard1 = cardList1[maxValue1Index].getSuit();
		int suitOfHighestCard2 = cardList2[maxValue2Index].getSuit();
		
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

		List<int> valueList1 = new List<int>();
		List<int> valueList2 = new List<int>();

		for (int i = 0; i < cardList1.Count; i++)
		{
			if (cardList1[i].getValue() == 0)
			{
				//Account for when Aces are low in a straight or straight flush
				if (detectHandType(firstHand) != (int) handType.STRAIGHT_FLUSH && 
						detectHandType(firstHand) != (int) handType.STRAIGHT)
					valueList1.Add(13);
				else
					valueList1.Add(0);
			}
			else
				valueList1.Add(cardList1[i].getValue());

			if (cardList2[i].getValue() == 0)
			{
				if (detectHandType(secondHand) != (int) handType.STRAIGHT_FLUSH && 
						detectHandType(secondHand) != (int) handType.STRAIGHT)
					valueList2.Add(13);
				else
					valueList2.Add(0);
			}
			else
				valueList2.Add(cardList2[i].getValue());
		}

		valueList1.Sort();
		valueList1.Reverse();
		valueList2.Sort();
		valueList2.Reverse();
		
		for (int i = 0; i < valueList1.Count; i++)
		{
			if (valueList1[i] > valueList2[i])
				return -1;
			else if (valueList1[i] < valueList2[i])
				return 1;
		}

		return 0;
	}
}
}
