#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_assignments)]
#![allow(unused_imports)]
#![allow(unused_mut)]

mod card;
mod hand;
mod hand_identifier;
mod hand_sorter;

use crate::card::Card;
use crate::hand::Hand;

use rand::Rng;
use std::fs::File;
use std::io::{self, Read, BufRead};

fn main() {
    let mut hands: Vec<Hand> = vec!(Hand::new(), Hand::new(), Hand::new(), Hand::new(), Hand::new(), Hand::new());
    
    println!("*** P O K E R   H A N D   A N A L Y Z E R ***\n\n");

    let args: Vec<String> = std::env::args().collect();

    if args.len() == 2 {
        let file_path = &args[1];
        let _ = print_file(&file_path);
        let _ = deal_from_file(&mut hands, &file_path);

        if has_duplicate(&hands) {
            return
        }

        print_hands(&hands);
        assign_types(&mut hands);
        hand_sorter::sort_hands(&mut hands);
        print_ranked_hands(&hands);
    }
    else {
        let mut deck = create_deck();
        print_deck(&deck);
        deal_from_deck(&mut hands, &mut deck);
        print_hands(&hands);
        print_remaining_deck(&deck);
        assign_types(&mut hands);
        hand_sorter::sort_hands(&mut hands);
        print_ranked_hands(&hands);
    }
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

// =============== Testing functions ===============

fn print_file(path: &String) -> io::Result<()> {
    println!("*** USING TEST DECK ***\n");

    println!("*** File: {}", path);

    let mut file = File::open(path)?;

    let mut contents = String::new();

    file.read_to_string(&mut contents)?;

    println!("{}", contents);

    Ok(())
}

fn deal_from_file(hands: &mut Vec<Hand>, path: &String) -> io::Result<()> {
    let mut file = File::open(path)?;

    let reader = io::BufReader::new(file);

    for (index, line) in reader.lines().enumerate() {
        match line {
            Ok(content) => hands[index] = Hand::new_from_str(&content), 
            Err(e) => println!("Error reading line: {}", e),
        }
    }

    Ok(())
}

fn has_duplicate(hands: &Vec<Hand>) -> bool {
    let mut card_hashes: Vec<usize> = Vec::new();

    for hand in hands {
        for card in hand.get_sorted_cards() {
            let card_hash = card.get_value() * 10 + card.get_suit();

            if let Some(index) = card_hashes.iter().position(|&x| x == card_hash) {
                println!("*** ERROR - DUPLICATED CARD FOUND IN DECK ***\n");

                println!("*** Duplicate: {} ***", card.to_string());
                return true;
            }
            else {
                card_hashes.push(card_hash);
            }
        }
    }

    false
}

// =============== Common Functions ===============

fn print_hands(hands: &Vec<Hand>) {
    println!("*** Here are the six hands...");

    for hand in hands {
        println!("{}", hand.to_string());
    }
    println!();
}

fn assign_types(hands: &mut Vec<Hand>) {
    for hand in hands {
        hand_identifier::assign_type(hand)
    }
}

fn print_ranked_hands(hands: &Vec<Hand>) {
    println!("--- WINNING HAND ORDER ---");

    for hand in hands {
        println!("{} - {}", hand.to_string(), hand::HAND_MAP[hand.get_hand_type()])
    }
}
