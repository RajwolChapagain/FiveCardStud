use strict;
use warnings;
use FindBin qw($RealBin);
use lib $RealBin;
use Card;
use Hand;

main();

sub main {
    print "*** P O K E R   H A N D   A N A L Y Z E R ***\n\n\n";

    my @deck = ();
    my @hands = (Hand->new(), Hand->new(), Hand->new(), Hand->new(), Hand->new(), Hand->new());

    init_deck(\@deck);
    print_deck(@deck);
    deal_from_deck(\@hands,\@deck);
    print_hands(@hands);
    print_remaining_deck(@deck);
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
    print "\n";
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
