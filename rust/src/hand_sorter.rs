use crate::hand::Hand;
use crate::card::Card;

pub fn sort_hands(hands: &mut Vec<Hand>) {
    sort_by_type(hands);
}

fn sort_by_type(hands: &mut Vec<Hand>) {
    hands.sort();
    hands.reverse();
}
