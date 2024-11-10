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

sub init_deck {
    my $deck_ref = shift;

    for my $i (0..51) {
        push @{$deck_ref}, Card->new($i % 13, $i / 13);
    }
}

sub print_deck {
    my @deck = @_;


    for my $i (0..$#deck) {
        print $deck[$i]->to_string . " ";

        if ( ($i + 1) % 13 == 0) {
            print "\n";
        }
    }
}
