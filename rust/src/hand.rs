use std::cmp::Ordering;
use itertools::Itertools;
use crate::card::Card;

pub const HAND_SIZE: usize = 5;
pub const HAND_MAP: [&str; 10] = ["High Card", "Pair", "Two Pair", "Three of a Kind", "Straight", "Flush", "Full House", "Four of a Kind", "Straight Flush", "Royal Straight Flush"];

pub struct Hand {
    hand_type: usize,
    cards: Vec<Card>,
}

impl Hand {
    pub fn new() -> Self {
        Self {hand_type: 0, cards: Vec::new() }
    }

    pub fn new_from_str(line: &str) -> Self {
        let split: Vec<&str> = line.split(',').collect();        

        let mut hand = Self::new();

        for token in split {
            hand.add_card(Card::new_from_str(token));
        }

        hand
    }

    pub fn add_card(&mut self, card: Card) {
        self.cards.push(card);
    }

    pub fn get_sorted_cards(&self) -> Vec<Card> {
        self.cards.iter().sorted().cloned().collect()
    }

    pub fn get_hand_type(&self) -> usize {
        self.hand_type
    }
    
    pub fn set_hand_type(&mut self, hand_type: usize) {
        self.hand_type = hand_type;
    }

    pub fn to_string(&self) -> String {
        let mut result = String::new();

        for card in &self.cards {
            result.push_str(&(format!("{:<4}", &card.to_string())));
        }

        result
    }
}

impl PartialEq for Hand {
    fn eq(&self, other: &Self) -> bool {
        self.get_hand_type() == other.get_hand_type()
    }
}

impl Eq for Hand {}

impl PartialOrd for Hand {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.get_hand_type().cmp(&other.get_hand_type()))
    }
}

impl Ord for Hand {
    fn cmp(&self, other: &Self) -> Ordering {
        self.get_hand_type().cmp(&other.get_hand_type())
    }
}
