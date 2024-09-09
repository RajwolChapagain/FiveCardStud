public class Card
{
	private final static String[] valueMap = {"A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"};
	private final static String[] suitMap = {"D", "C", "H", "S"};

	private final int value;
	private final int suit;

	public Card(int value, int suit)
	{
		this.value = value;
		this.suit = suit;	
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
}
