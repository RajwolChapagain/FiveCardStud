package Card;

use strict;
use warnings;

our @VALUE_MAP = ("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K");
our @SUIT_MAP = ("D", "C", "H", "S");

sub new {
    my ($class, @args) = @_;

    if (@args == 2) {
        my ($value, $suit) = @args;

        my $self = bless {
            value => $value,
            suit => $suit
        }, $class;

        return $self;
    } elsif (@args == 1) {              # If a string is passed in
        my $string = $args[0];
        my ($value_string, $suit_string);
        my ($value, $suit);

        $string =~ s/^\s+//;            # Remove preceding spaces

        if (length($string) == 3) {     # If it's a 10 card
            $value_string = substr($string, 0, 2); 
            $suit_string = substr($string, -1, 1); 

        } elsif (length($string) == 2) {
            $value_string = substr($string, 0, 1); 
            $suit_string = substr($string, -1, 1); 

        }

        $value = get_value_index($value_string);
        $suit = get_suit_index($suit_string);

        if ($value == -1) {
            die "Error: Card can't have value ", $value_string;
        }

        if ($suit == -1) {
            die "Error: Card can't have suit ", $suit_string;
        }

        my $self = bless {
            value => $value,
            suit => $suit
        }, $class;

        return $self;
         
    } elsif (@args == 0) {              # By default
        my $self = bless {
            value => 0,
            suit => 0
        }, $class;

        return $self;
    }
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

sub get_value_index {
    my $value_string = shift; 
    
    for my $i (0..$#VALUE_MAP) {
        if ($VALUE_MAP[$i] eq $value_string) {
            return $i;
        }
    }

    return -1;
}

sub get_suit_index {
    my $suit_string = shift; 
    
    for my $i (0..$#SUIT_MAP) {
        if ($SUIT_MAP[$i] eq $suit_string) {
            return $i;
        }
    }

    return -1;
}

return 1;
