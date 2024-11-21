use crate::hand::Hand;
use crate::card::Card;

pub fn assign_type(hand: &mut Hand) {
    let cards = hand.get_sorted_cards();

    if is_royal_straight_flush(&cards) {
        hand.set_hand_type(9);
    }
    else if is_flush(&cards) {
        hand.set_hand_type(5);
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

fn is_flush(cards: &Vec<Card>) -> bool {
    let prev_suit: usize = cards[0].get_suit();

    for card in cards {
        if card.get_suit() != prev_suit {
            return false;
        }
    }

    true
}
