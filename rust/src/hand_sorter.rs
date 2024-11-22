use crate::card::Card;
use crate::hand;
use crate::hand::Hand;
use crate::hand_identifier;

const COMPARATORS: [fn(&Hand, &Hand) -> bool; 10] = [compare_royal_flush,compare_pair, compare_two_pair, compare_three_of_a_kind, compare_straight, compare_flush, compare_full_house, compare_four_of_a_kind, compare_straight_flush, compare_royal_flush];

pub fn sort_hands(hands: &mut Vec<Hand>) {
    sort_by_type(hands);
    sort_ties(hands);
}

fn sort_by_type(hands: &mut Vec<Hand>) {
    hands.sort();
    hands.reverse();
}

fn sort_ties(hands: &mut Vec<Hand>) {
    let mut start_index = 0;
    let mut last_type = hands[0].get_hand_type();
    let mut subarray_indices: Vec<(usize, usize)> = Vec::new();

    for (i, hand) in hands.iter().enumerate() {
        if hand.get_hand_type() != last_type {
            subarray_indices.push((start_index,i-1));

            start_index = i;
            last_type = hands[i].get_hand_type();
        }
        else if i == hands.len() - 1 {
            subarray_indices.push((start_index,i));
        }
    }

    for indices in subarray_indices {
        sort_subarray(&mut hands[indices.0..=indices.1]);
    }
}

fn sort_subarray(hands: &mut [Hand]) {
    let hand_type = hands[0].get_hand_type();

    let comparator = COMPARATORS[hand_type];

    hands.sort_by(|h1, h2| {
        if comparator(h1, h2) {
            std::cmp::Ordering::Greater
        } else {
            std::cmp::Ordering::Less
        }
    });

}

// =============== Comparators ===============
// All return:
// true if l1 < l2
// false if l1 > l2

fn compare_royal_flush(h1: &Hand, h2: &Hand) -> bool {
    let l1 = h1.get_sorted_cards();
    let l2 = h2.get_sorted_cards();

    l1[0].get_suit() < l2[0].get_suit()
}

fn compare_straight_flush(h1: &Hand, h2: &Hand) -> bool {
    let l1 = h1.get_sorted_cards();
    let l2 = h2.get_sorted_cards();

    let highest_card_comparison = compare_highest_card(&l1, &l2);

    if highest_card_comparison == 1 {
        return true;
    }
    else if highest_card_comparison == -1 {
        return false;
    }

    l1[0].get_suit() < l2[0].get_suit()
}

fn compare_four_of_a_kind(h1: &Hand, h2: &Hand) -> bool {
    let l1 = h1.get_sorted_cards();
    let l2 = h2.get_sorted_cards();

    let c1 = &get_cards_occuring_n_times(&l1, 4)[0];
    let c2 = &get_cards_occuring_n_times(&l2, 4)[0];

    let mut value1 = c1.get_value();
    let mut value2 = c2.get_value();

    if value1 == 0 {
        value1 = 13;
    }

    if value2 == 0 {
        value2 = 13;
    }

    value1 < value2
}

fn compare_full_house(h1: &Hand, h2: &Hand) -> bool {
    let l1 = h1.get_sorted_cards();
    let l2 = h2.get_sorted_cards();

    let c1 = &get_cards_occuring_n_times(&l1, 3)[0];
    let c2 = &get_cards_occuring_n_times(&l2, 3)[0];

    let mut value1 = c1.get_value();
    let mut value2 = c2.get_value();

    if value1 == 0 {
        value1 = 13;
    }

    if value2 == 0 {
        value2 = 13;
    }

    value1 < value2
}

fn compare_flush(h1: &Hand, h2: &Hand) -> bool {
    let l1 = h1.get_sorted_cards();
    let l2 = h2.get_sorted_cards();

    let highest_card_comparison = compare_highest_card(&l1, &l2);

    if highest_card_comparison == 1 {
        return true;
    }
    else if highest_card_comparison == -1 {
        return false;
    }

    l1[0].get_suit() < l2[0].get_suit()
}

fn compare_straight(h1: &Hand, h2: &Hand) -> bool {
    let l1 = h1.get_sorted_cards();
    let l2 = h2.get_sorted_cards();

    let highest_card_comparison = compare_highest_card(&l1, &l2);

    if highest_card_comparison == 1 {
        return true;
    }
    else if highest_card_comparison == -1 {
        return false;
    }

    let mut highest_card_suit1 = l1[l1.len() - 1].get_suit();
    let mut highest_card_suit2 = l2[l2.len() - 1].get_suit();

    if hand_identifier::is_royal_straight(&l1) {
        highest_card_suit1 = l1[0].get_suit();
    }

    if hand_identifier::is_royal_straight(&l2) {
        highest_card_suit2 = l2[0].get_suit();
    }

    highest_card_suit1 < highest_card_suit2
}

fn compare_three_of_a_kind(h1: &Hand, h2: &Hand) -> bool {
    let l1 = h1.get_sorted_cards();
    let l2 = h2.get_sorted_cards();

    let c1 = &get_cards_occuring_n_times(&l1, 3)[0];
    let c2 = &get_cards_occuring_n_times(&l2, 3)[0];

    let mut value1 = c1.get_value();
    let mut value2 = c2.get_value();

    if value1 == 0 {
        value1 = 13;
    }

    if value2 == 0 {
        value2 = 13;
    }

    value1 < value2
}

fn compare_two_pair(h1: &Hand, h2: &Hand) -> bool {
    let l1 = h1.get_sorted_cards();
    let l2 = h2.get_sorted_cards();

    let pairs1 = get_cards_occuring_n_times(&l1, 2);
    let pairs2 = get_cards_occuring_n_times(&l2, 2);

    let highest_card_comparison = compare_highest_card(&pairs1, &pairs2);

    if highest_card_comparison == 1 {
        return true;
    }
    else if highest_card_comparison == -1 {
        return false;
    }

    let kicker1 = get_cards_occuring_n_times(&l1, 1);
    let kicker2 = get_cards_occuring_n_times(&l2, 1);

    let kicker_card_comparison = compare_highest_card(&kicker1, &kicker2);

    if kicker_card_comparison == 1 {
        return true;
    }
    else if kicker_card_comparison == -1 {
        return false;
    }

    let kicker_card1 = &kicker1[0];
    let kicker_card2 = &kicker2[0];

    kicker_card1.get_suit() < kicker_card2.get_suit()
}


fn compare_pair(h1: &Hand, h2: &Hand) -> bool {
    let l1 = h1.get_sorted_cards();
    let l2 = h2.get_sorted_cards();

    let pair1 = get_cards_occuring_n_times(&l1, 2);
    let pair2 = get_cards_occuring_n_times(&l2, 2);

    let highest_card_comparison = compare_highest_card(&pair1, &pair2);

    if highest_card_comparison == 1 {
        return true;
    }
    else if highest_card_comparison == -1 {
        return false;
    }

    let singles1 = get_cards_occuring_n_times(&l1, 1);
    let singles2 = get_cards_occuring_n_times(&l2, 1);

    let highest_single_comparison = compare_highest_card(&singles1, &singles2);

    if highest_single_comparison == 1 {
        return true;
    }
    else if highest_single_comparison == -1 {
        return false;
    }

    let mut highest_single_suit1 = singles1[singles1.len() - 1].get_suit();
    let mut highest_single_suit2 = singles2[singles2.len() - 1].get_suit();

    if singles1[0].get_value() == 0 {
        highest_single_suit1 = singles1[0].get_suit();
    }

    if singles2[0].get_value() == 0 {
        highest_single_suit2 = singles2[0].get_suit();
    }

    highest_single_suit1 < highest_single_suit2
}

// =============== Helpers ===============

// Returns:
//  1 if l1 < l2
// -1 if l1 > l2
//  0 if l1 == l2
fn compare_highest_card(l1: &Vec<Card>, l2:&Vec<Card>) -> i32 {
    let mut value_list1: Vec<usize> = Vec::new();
    let mut value_list2: Vec<usize> = Vec::new();


    for i in 0..l1.len() {
        value_list1.push(l1[i].get_value());

        if l1[i].get_value() == 0 {
            value_list1[i] = 13;

            if l1.len() == hand::HAND_SIZE {
                if hand_identifier::is_straight(l1) && !hand_identifier::is_royal_straight(l1) {
                    value_list1[i] = 0;
                }
            }
        }

        value_list2.push(l2[i].get_value());

        if l2[i].get_value() == 0 {
            value_list2[i] = 13;

            if l2.len() == hand::HAND_SIZE {
                if hand_identifier::is_straight(l2) && !hand_identifier::is_royal_straight(l2) {
                    value_list2[i] = 0;
                }
            }
        }
    }

    value_list1.sort();
    value_list2.sort();

    value_list1.reverse();
    value_list2.reverse();

    for (value1, value2) in value_list1.iter().zip(value_list2.iter()) {
        if value1 < value2 {
            return 1;
        }
        else if value1 > value2 {
            return -1;
        }
    }

    0
}

pub fn get_cards_occuring_n_times(card_list: &Vec<Card>, n: usize) -> Vec<Card> {    
    let mut result: Vec<Card> = Vec::new();

    for card in card_list {
        if !result.contains(card) {
            if card_list.iter().filter(|&x| x == card).count() == n {
                result.push(card.clone())
            }
        }
    }

    result
}
