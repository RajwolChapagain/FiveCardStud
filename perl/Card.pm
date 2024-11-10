package Card;

use strict;
use warnings;

our @VALUE_MAP = ("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K");
our @SUIT_MAP = ("D", "C", "H", "S");

sub new {
    my $class = shift;
    my ($value, $suit) = @_;

    my $self = bless {
        value => $value,
        suit => $suit
    }, $class;

    return $self;
}

sub to_string {
    my $self = shift;
    
    return $VALUE_MAP[$self->get_value] . $SUIT_MAP[$self->get_suit];
}

sub get_value {
    my $self = shift;

    return $self->{value};
}

sub get_suit {
    my $self = shift;

    return $self->{suit};
}

return 1;
