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
    else if is_four_of_a_kind(&cards) {
        hand.set_hand_type(7);
    }
    else if is_full_house(&cards) {
        hand.set_hand_type(6);
    }
    else if is_flush(&cards) {
        hand.set_hand_type(5);
    }
    else if is_straight(&cards) {
        hand.set_hand_type(4);
    }
    else if is_three_of_a_kind(&cards) {
        hand.set_hand_type(3);
    }
    else if is_two_pair(&cards) {
        hand.set_hand_type(2);
    }
    else if is_pair(&cards) {
        hand.set_hand_type(1);
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

pub fn is_royal_straight(cards: &Vec<Card>) -> bool {
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


fn is_four_of_a_kind(cards: &Vec<Card>) -> bool {
    let set = get_frequency_set(&cards);

    if set.contains(&4) {
        return true;
    }

    false
}

fn is_full_house(cards: &Vec<Card>) -> bool {
    let set = get_frequency_set(&cards);

    if set.contains(&3) && set.contains(&2) {
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

pub fn is_straight(cards: &Vec<Card>) -> bool {
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

fn is_three_of_a_kind(cards: &Vec<Card>) -> bool {
    let set = get_frequency_set(&cards);

    if set.contains(&3) {
        return true;
    }

    false
}

fn is_two_pair(cards: &Vec<Card>) -> bool {
    let set = get_frequency_set(&cards);

    let two_count = set.iter().filter(|&&x| x == 2).count();

    if two_count == 2 {
        return true;
    }

    false
}

fn is_pair(cards: &Vec<Card>) -> bool {
    let set = get_frequency_set(&cards);

    if set.contains(&2) {
        return true;
    }

    false
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
