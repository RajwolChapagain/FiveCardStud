use strict;
use warnings;
use FindBin qw($RealBin);
use lib $RealBin;
use Card;
use Hand;

main();

sub main {
    my @deck = ();

    init_deck(\@deck);
    print_deck(@deck);

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
}
