public class Card implements Comparable<Card>
{
	private final static String[] valueMap = {"A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"};
	private final static String[] suitMap = {"D", "C", "H", "S"};

	private final int value;
	private final int suit;

	public Card(int value, int suit)
	{
		if (value < 0 || value >= valueMap.length || suit < 0 || suit >= suitMap.length)
			throw new InvalidCardException("A card can't have a value index of " + value + " and/or a suit index of " + suit);
		this.value = value;
		this.suit = suit;	
	}

	public Card(String valueString, String suitString)
	{
		this(getIndexOfValue(valueString), getIndexOfSuit(suitString));
	}

	public int getValue()
	{
		return value;
	}

	public int getSuit()
	{
		return suit;
	}

	public void printCard()
	{
		System.out.print(valueMap[value] + suitMap[suit]);
	}

	// This function returns -1 if this card is smaller, 0 if the cards are equal, and 1 if this card is larger
	@Override
	public int compareTo(Card other)
	{
		return Integer.compare(this.value, other.value);
	}

	public static int getIndexOfValue(String value)
	{
		for (int i = 0; i < valueMap.length; i++)
			if (valueMap[i].equals(value))
				return i;
		return -1;
	}

	public static int getIndexOfSuit(String suit)
	{
		for (int i = 0; i < suitMap.length; i++)
			if (suitMap[i].equals(suit))
				return i;
		return -1;
	}
}

class InvalidCardException extends RuntimeException {

	public InvalidCardException(String message)
	{
		super(message);
	}
}
