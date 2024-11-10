use strict;
use warnings;
use FindBin qw($RealBin);
use lib $RealBin;
use Card;
use Hand;
use HandIdentifier;
use HandSorter;

main();

sub main {
    print "*** P O K E R   H A N D   A N A L Y Z E R ***\n\n\n";

    my @hands = (Hand->new(), Hand->new(), Hand->new(), Hand->new(), Hand->new(), Hand->new());


    if (@ARGV == 0) {
        my @deck = ();
        init_deck(\@deck);
        print_deck(@deck);
        deal_from_deck(\@hands,\@deck);
        print_hands(@hands);
        print_remaining_deck(@deck);
        assign_types(\@hands);
        HandSorter::sort_hands(\@hands);
        print_ranked_hands(@hands);
    } else {
        my $file_path = $ARGV[0];
        print_file($file_path);
        deal_from_file(\@hands, $file_path);
        check_duplicate(@hands);
        print_hands(@hands);
        assign_types(\@hands);
        HandSorter::sort_hands(\@hands);
        print_ranked_hands(@hands);
    }
}

# =============== Non-testing subroutines ===============

sub init_deck {
    my $deck_ref = shift;

    for my $i (0..51) {
        push @{$deck_ref}, Card->new($i % 13, $i / 13);
    }

    # Shuffle deck
    for my $i (0..$#$deck_ref) {
        my $rand_int = int(rand(52));

        my $temp = @$deck_ref[$i];
        @$deck_ref[$i] = @$deck_ref[$rand_int];
        @$deck_ref[$rand_int] = $temp;
    }

}

sub print_deck {
    my @deck = @_;

    print "*** USING RANDOMIZED DECK OF CARDS ***\n\n";

    print "*** Shuffled 52 card deck:\n";
    for my $i (0..$#deck) {
        printf("%-4s", $deck[$i]->to_string);

        if ( ($i + 1) % 13 == 0) {
            print "\n";
        }
    }

    print "\n";
}

sub deal_from_deck {
    my $hand_ref = shift;
    my $deck_ref = shift;

    for my $i (0..4) {
        for my $i (0..$#$hand_ref) {
            @$hand_ref[$i]->add_card(shift @$deck_ref);
        }
    }
}

sub print_remaining_deck {
    my @deck = @_;

    print "*** Here is what remains in the deck...\n";
    foreach my $card (@deck) {
        printf("%-4s", $card->to_string);
    }
    print "\n\n";
}

# =============== Testing subroutines ===============

sub print_file {
    my $path = shift;
    
    print "*** USING TEST DECK ***\n\n";

    print "*** File: $path\n";

    open(my $in,  "<",  $path);


    while (<$in>) {
        print $_;
    }
    print "\n";
}

sub deal_from_file {
    my ($hands_ref, $path) = @_;

    open(my $in, "<", $path);

    foreach my $hand (@$hands_ref) {
        $hand = Hand->new(scalar(<$in>));
    }
}

sub check_duplicate {
    my @hands = @_;

    my @hashes = ();

    foreach my $hand (@hands) {
        foreach my $card ($hand->get_sorted_cards) {
            my $card_hash = $card->get_value * 10 + $card->get_suit;

            foreach my $hash (@hashes) {
                if ($hash == $card_hash) {
                    die "*** ERROR - DUPLICATED CARD FOUND IN DECK ***\n\n*** Duplicate: " . $card->to_string . " ***\n";
                }
            }

            push @hashes, $card_hash;
        }
    }
}

# =============== Common subroutines ===============

sub print_hands {
    my @hands = @_;

    print "*** Here are the six hands...\n";

    foreach my $hand (@hands) {
        print $hand->to_string . "\n";
    }
    print "\n";
}

sub assign_types {
    my $hands_ref = shift;

    foreach my $hand (@$hands_ref) {
        HandIdentifier::assign_type($hand)
    }
}

sub print_ranked_hands {
    my @hands = @_;

    print "--- WINNING HAND ORDER ---\n";
    foreach my $hand (@hands) {
        print $hand->to_string . "- " . @Hand::HAND_MAP[$hand->get_type] . "\n"; 
    }
    print "\n";
}
