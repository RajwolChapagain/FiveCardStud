use crate::hand::Hand;
use crate::card::Card;

const COMPARATORS: [fn(&Hand, &Hand) -> bool; 10] = [compare_royal_flush,compare_royal_flush, compare_royal_flush, compare_royal_flush, compare_royal_flush, compare_royal_flush, compare_royal_flush, compare_royal_flush, compare_royal_flush, compare_royal_flush];

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
// true if l1 is weaker
// false if l1 is stronger

fn compare_royal_flush(h1: &Hand, h2: &Hand) -> bool {
    let l1 = h1.get_sorted_cards();
    let l2 = h2.get_sorted_cards();

    l1[0].get_suit() < l2[0].get_suit()
}
