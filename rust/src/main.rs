#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_assignments)]
#![allow(unused_imports)]
#![allow(unused_mut)]

mod card;
mod hand;

use crate::card::Card;
use crate::hand::Hand;
use rand::Rng;

fn main() {
    let mut hands: Vec<Hand> = vec!(Hand::new(), Hand::new(), Hand::new(), Hand::new(), Hand::new(), Hand::new());
    
    println!("*** P O K E R   H A N D   A N A L Y Z E R ***\n\n");

    let mut deck = create_deck();
    print_deck(&deck);
    deal_from_deck(&mut hands, &mut deck);
    print_hands(&hands);
    print_remaining_deck(&deck);
    print_ranked_hands(&hands);
}

// =============== Non-testing functions ===============

fn create_deck() -> Vec<Card> {
    let mut deck: Vec<Card> = Vec::new();

    for i in 0..=51 {
        deck.push(Card::new(i % 13, i / 13));
    }

    for i in 0..=51 {
        let rand_index = rand::thread_rng().gen_range(0..=51);
        deck.swap(i, rand_index);
    }

    deck
}

fn print_deck(deck: &Vec<Card>) {
    println!("*** USING RANDOMIZED DECK OF CARDS ***\n");
    println!("*** Shuffled 52 card deck:");

    let mut i = 0;
    for card in deck {
        print!("{:<4}", card.to_string());

        if (i + 1) % 13 == 0 {
            println!();
        }

        i += 1;
    }
    println!();
}

fn print_remaining_deck(deck: &Vec<Card>) {
    println!("*** Here is what remains in the deck...");
    
    for card in deck {
        print!("{:<4}", card.to_string());
    }

    println!("\n");
}

fn deal_from_deck(hands: &mut Vec<Hand>, deck: &mut Vec<Card>) {
    for i in 0..hand::HAND_SIZE {
        for hand in &mut *hands {
            hand.add_card(deck.remove(0));
        }
    }
}

// =============== Common Functions ===============

fn print_hands(hands: &Vec<Hand>) {
    println!("*** Here are the six hands...");

    for hand in hands {
        println!("{}", hand.to_string());
    }
    println!();
}

fn print_ranked_hands(hands: &Vec<Hand>) {
    println!("--- WINNING HAND ORDER ---");

    for hand in hands {
        println!("{} - {}", hand.to_string(), hand::HAND_MAP[hand.get_hand_type()])
    }
    
    println!();
}
