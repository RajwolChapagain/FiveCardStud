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
    let hands: Vec<Hand> = Vec::new();
    println!("*** P O K E R   H A N D   A N A L Y Z E R ***\n\n");

    let mut deck = create_deck();
    print_deck(&deck);
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
}
