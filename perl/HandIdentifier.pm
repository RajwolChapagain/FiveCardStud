package HandIdentifier;

use strict;
use warnings;

sub assign_type {
    my $hand = shift;

    my @cards = $hand->get_sorted_cards;

    if (is_royal_straight_flush(@cards)) {
        $hand->set_type(9);
    } elsif (is_straight_flush(@cards)) {
        $hand->set_type(8);
    } elsif (is_flush(@cards)) {
        $hand->set_type(5);
    } elsif (is_straight(@cards)) {
        $hand->set_type(4);
    } else {
        $hand->set_type(0);
    }
}

sub is_royal_straight_flush {
    my @cards = @_;

    if (is_royal_straight(@cards) and is_flush(@cards)) {
        return 1;
    }

   return 0;
}

sub is_royal_straight {
    my @cards = @_;

    if ($cards[0]->get_value == 0 and $cards[1]->get_value == 9 and $cards[2]->get_value == 10 and $cards[3]->get_value == 11 and $cards[4]->get_value == 12) {
        return 1;
    }
    
    return 0;
}

sub is_straight_flush {
    my @cards = @_;

    if (is_straight(@cards) and is_flush(@cards)) {
        return 1;
    }

    return 0;
}

sub is_flush {
    my @cards = @_;

    my $prev_suit = $cards[0]->get_suit;

    foreach my $card (@cards) {
        if ($card->get_suit != $prev_suit) {
            return 0;
        }
    }

    return 1;
}

sub is_straight {
    my @cards = @_;
    
    if (is_royal_straight(@cards)) {
        return 1;
    }

    my $prev_value = $cards[0]->get_value - 1;

    foreach my $card (@cards) {
        my $curr_value = $card->get_value;

        if ($curr_value != $prev_value + 1) {
            return 0;
        }

        $prev_value = $curr_value;
    }

    return 1;
}

return 1;

# =============== Helpers ===============

sub get_frequency_set {
    my @cards = @_;

    my @frequency_set = ();
    my $prev_value = -1;

    foreach my $card (@cards) {
        if ($card->get_value != $prev_value) {
            push @frequency_set, get_card_count($card, @cards);
            $prev_value = $card->get_value;
        }


    }

    return sort @frequency_set;
}

sub get_card_count {
    my ($card, @card_list) = @_;

    my $count = 0;

    foreach my $c (@card_list) {
        if ($c->get_value == $card->get_value) {
            $count += 1;
        }
    }

    return $count;
}
