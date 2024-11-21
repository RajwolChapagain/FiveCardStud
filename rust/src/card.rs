use std::cmp::Ordering;

pub const VALUE_MAP: [&str; 13] = ["A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"];
pub const SUIT_MAP: [&str; 4] = ["D", "C", "H", "S"];

#[derive(Clone)]
pub struct Card {
    value: usize,
    suit: usize,
}

impl Card {
    pub fn new(value: usize, suit: usize) -> Self {
        Self {value, suit}
    }

    pub fn new_from_str(string: &str) -> Self {
        let card_string = String::from(string.trim());
        
        let value = &card_string[..card_string.len() - 1];
        let suit = &card_string[card_string.len() - 1..];

        let mut value_index: usize = 0;
        let mut suit_index: usize = 0;

        if let Some(index) = VALUE_MAP.iter().position(|&x| x == value) {
            value_index = index; 
        }
        else {
            println!("Error: Can't instantiate card with value: {value}");
            std::process::exit(1);
        }

        if let Some(index) = SUIT_MAP.iter().position(|&x| x == suit) {
            suit_index = index; 
        }
        else {
            println!("Error: Can't instantiate card with suit: {suit}");
            std::process::exit(1);
        }

        Self::new(value_index, suit_index)
    }

    pub fn get_value(&self) -> usize {
        self.value
    }
    
    pub fn get_suit(&self) -> usize {
        self.suit
    }

    pub fn to_string(&self) -> String {
        let mut result = String::from(VALUE_MAP[self.get_value()]);
        result.push_str(SUIT_MAP[self.get_suit()]);
        result
    }
}

impl PartialEq for Card {
    fn eq(&self, other: &Self) -> bool {
        self.get_value() == other.get_value()
    }
}

impl Eq for Card {}

impl PartialOrd for Card {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.get_value().cmp(&other.get_value()))
    }
}

impl Ord for Card {
    fn cmp(&self, other: &Self) -> Ordering {
        self.get_value().cmp(&other.get_value())
    }
}
