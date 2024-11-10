package Hand;

use strict;
use warnings;
use Card;

our $HAND_SIZE = 5;
our @HAND_MAP = ("High Card", "Pair", "Two Pair", "Three of a Kind", "Straight", "Flush", "Full House", "Four of a Kind", "Straight Flush", "Royal Straight Flush");

sub new {
    my ($class, @args) = @_;

    my @cards = ();
    my $self = bless {
        cards => \@cards,
        type => -1
    }, $class;

    if (@args == 1) {  # If a string of comma-separated cards is passed in
        my @tokens = split(",", $args[0]);

        foreach my $token (@tokens) {
            $self->add_card(Card->new($token)); 
        }
    }

    return $self;
}

sub add_card {
    my $self = shift;

    push @{$self->{cards}}, shift;
}

sub to_string {
    my $self = shift;

    my $result = "";

    foreach my $card (@{$self->{cards}}) {
        $result .= sprintf("%-4s", $card->to_string);
    }

    return $result;
}

sub get_type {
    my $self = shift;

    return $self->{type};
}

sub set_type {
    my $self = shift;

    $self->{type} = shift;
}

sub get_sorted_cards {
    my $self = shift;

    return sort {$a->get_value <=> $b->get_value} @{$self->{cards}};
}

return 1;
