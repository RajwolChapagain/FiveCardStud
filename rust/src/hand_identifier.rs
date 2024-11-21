use crate::hand::Hand;
use crate::card::Card;

pub fn assign_type(hand: &mut Hand) {
    let cards = hand.get_sorted_cards();

    if is_royal_straight_flush(&cards) {
        hand.set_hand_type(9);
    }
    else if is_straight_flush(&cards) {
        hand.set_hand_type(8);
    }
    else if is_flush(&cards) {
        hand.set_hand_type(5);
    }
    else if is_straight(&cards) {
        hand.set_hand_type(4);
    }
    else {
        hand.set_hand_type(0);
    }
}

fn is_royal_straight_flush(cards: &Vec<Card>) -> bool {
    if is_royal_straight(&cards) && is_flush(&cards) {
        return true;
    }

    false
}

fn is_royal_straight(cards: &Vec<Card>) -> bool {
    if cards[0].get_value() == 0 && cards[1].get_value() == 9 && cards[2].get_value() == 10 &&
        cards[3].get_value() == 11 && cards[4].get_value() == 12 {
            return true;
    }

    false
}

fn is_straight_flush(cards: &Vec<Card>) -> bool {
    if is_straight(&cards) && is_flush(&cards) {
        return true;
    }

    false
}

fn is_flush(cards: &Vec<Card>) -> bool {
    let prev_suit: usize = cards[0].get_suit();

    for card in cards {
        if card.get_suit() != prev_suit {
            return false;
        }
    }

    true
}

fn is_straight(cards: &Vec<Card>) -> bool {
    if is_royal_straight(&cards) {
        return true;
    }

    let mut prev_value = cards[0].get_value();

    for i in 1..cards.len() {
        let curr_value = cards[i].get_value();

        if curr_value != prev_value + 1 {
            return false;
        }

        prev_value = curr_value;
    }

    true
}

// ===============  Helpers ===============

fn get_frequency_set(cards: &Vec<Card>) -> Vec<usize> {
    let mut set: Vec<usize> = Vec::new();

    let mut prev_value = 100;

    for card in cards {
        if card.get_value() == prev_value {
            continue
        }
        
        set.push(cards.iter().filter(|&x| x == card).count());
        prev_value = card.get_value();
    }

    set.sort();
    set
}
