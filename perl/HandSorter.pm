package HandSorter;

use strict;
use warnings;
use HandIdentifier;

my @comparators = (\&compare_royal_flush, \&compare_pair, \&compare_two_pair, \&compare_three_of_a_kind, \&compare_straight, \&compare_flush, \&compare_full_house, \&compare_four_of_a_kind, \&compare_straight_flush, \&compare_royal_flush);


sub sort_hands {
    my $hand_ref = shift;

    sort_by_type($hand_ref);
    sort_ties($hand_ref);
}

sub sort_by_type {
    my $hand_ref = shift;

    @$hand_ref = sort { $b->get_type <=> $a->get_type } @$hand_ref;
}

sub sort_ties {
    my $hand_ref = shift;

    my $start_index = 0;
    my $last_type = @$hand_ref[0]->get_type;

    for my $i (0..$#$hand_ref) {
        if (@$hand_ref[$i]->get_type != $last_type) {
            @$hand_ref[$start_index..$i-1] = sort { $comparators[$last_type]($a, $b) } @$hand_ref[$start_index..$i-1];

            $start_index = $i;
            $last_type = @$hand_ref[$i]->get_type;
        } elsif ($i == $#$hand_ref) {
                @$hand_ref[$start_index..$i] = sort { $comparators[$last_type]($a, $b) } @$hand_ref[$start_index..$i];
        }
    }
}

# =============== Comparators ===============
# All return:
#  1 if h1 is weaker
# -1 if h1 is stronger

sub compare_royal_flush {
    my ($h1, $h2) = @_;

    my @l1 = $h1->get_sorted_cards;
    my @l2 = $h2->get_sorted_cards;

    if ($l1[0]->get_suit < $l2[0]->get_suit) {
        return 1;
    }

    return -1;
}

sub compare_straight_flush {
    my ($h1, $h2) = @_;

    my @l1 = $h1->get_sorted_cards;
    my @l2 = $h2->get_sorted_cards;

    my $highest_card_comparison = compare_highest_card(\@l1, \@l2);

    if ($highest_card_comparison == 1) {
        return 1;
    } elsif ($highest_card_comparison == -1) {
        return -1;
    }

    if ($l1[0]->get_suit < $l2[0]->get_suit) {
        return 1;
    }

    return -1;
}

sub compare_four_of_a_kind {
    my ($h1, $h2) = @_;

    my @l1 = $h1->get_sorted_cards;
    my @l2 = $h2->get_sorted_cards;

    my $c1 = (get_cards_occuring_n_times(\@l1, 4))[0];
    my $c2 = (get_cards_occuring_n_times(\@l2, 4))[0];

    my $value1 = $c1->get_value;
    my $value2 = $c2->get_value;


    if ($value1 == 0) {
            $value1 = 13;
    }

    if ($value2 == 0) {
            $value2 = 13;
    }

    if ($value1 < $value2) {
        return 1;
    }

    return -1;
}

sub compare_full_house {
    my ($h1, $h2) = @_;

    my @l1 = $h1->get_sorted_cards;
    my @l2 = $h2->get_sorted_cards;

    my $c1 = (get_cards_occuring_n_times(\@l1, 3))[0];
    my $c2 = (get_cards_occuring_n_times(\@l2, 3))[0];

    my $value1 = $c1->get_value;
    my $value2 = $c2->get_value;


    if ($value1 == 0) {
            $value1 = 13;
    }

    if ($value2 == 0) {
            $value2 = 13;
    }

    if ($value1 < $value2) {
        return 1;
    }

    return -1;
}

sub compare_flush {
    my ($h1, $h2) = @_;

    my @l1 = $h1->get_sorted_cards;
    my @l2 = $h2->get_sorted_cards;

    my $highest_card_comparison = compare_highest_card(\@l1, \@l2);

    if ($highest_card_comparison == 1) {
        return 1;
    } elsif ($highest_card_comparison == -1) {
        return -1;
    }

    if ($l1[0]->get_suit < $l2[0]->get_suit) {
        return 1;
    }

    return -1;
}

sub compare_straight {
    my ($h1, $h2) = @_;

    my @l1 = $h1->get_sorted_cards;
    my @l2 = $h2->get_sorted_cards;

    my $highest_card_comparison = compare_highest_card(\@l1, \@l2);

    if ($highest_card_comparison == 1) {
        return 1;
    } elsif ($highest_card_comparison == -1) {
        return -1;
    }

    my $highest_card_suit1 = -1;
    my $highest_card_suit2 = -1;

    if (!HandIdentifier::is_royal_straight(@l1)) {
        $highest_card_suit1 = $l1[$#l1]->get_suit;
    } else {
        $highest_card_suit1 = $l1[0]->get_suit;
    }

    if (!HandIdentifier::is_royal_straight(@l2)) {
        $highest_card_suit2 = $l2[$#l2]->get_suit;
    } else {
        $highest_card_suit2 = $l2[0]->get_suit;
    }

    if ($highest_card_suit1 < $highest_card_suit2) {
        return 1;
    }

    return -1;
}

sub compare_three_of_a_kind {
    my ($h1, $h2) = @_;

    my @l1 = $h1->get_sorted_cards;
    my @l2 = $h2->get_sorted_cards;

    my $c1 = (get_cards_occuring_n_times(\@l1, 3))[0];
    my $c2 = (get_cards_occuring_n_times(\@l2, 3))[0];

    my $value1 = $c1->get_value;
    my $value2 = $c2->get_value;


    if ($value1 == 0) {
            $value1 = 13;
    }

    if ($value2 == 0) {
            $value2 = 13;
    }

    if ($value1 < $value2) {
        return 1;
    }

    return -1;
}

sub compare_two_pair {
    my ($h1, $h2) = @_;

    my @l1 = $h1->get_sorted_cards;
    my @l2 = $h2->get_sorted_cards;

    my @pairs1 = get_cards_occuring_n_times(\@l1, 2);
    my @pairs2 = get_cards_occuring_n_times(\@l2, 2);

    my $highest_card_comparison = compare_highest_card(\@pairs1, \@pairs2);

    if ($highest_card_comparison == 1) {
        return 1;
    } elsif ($highest_card_comparison == -1) {
        return -1;
    }

    my @kicker1 = get_cards_occuring_n_times(\@l1, 1);
    my @kicker2 = get_cards_occuring_n_times(\@l2, 1);

    my $kicker_card_comparison = compare_highest_card(\@kicker1, \@kicker2);

    if ($kicker_card_comparison == 1) {
        return 1;
    } elsif ($kicker_card_comparison == -1) {
        return -1;
    }

    my $kicker_card1 = $kicker1[0];
    my $kicker_card2 = $kicker2[0];

    if ($kicker_card1->get_suit < $kicker_card2->get_suit) {
        return 1;
    }

    return -1;
}

sub compare_pair {
    my ($h1, $h2) = @_;

    my @l1 = $h1->get_sorted_cards;
    my @l2 = $h2->get_sorted_cards;

    my @pair1 = get_cards_occuring_n_times(\@l1, 2);
    my @pair2 = get_cards_occuring_n_times(\@l2, 2);

    my $highest_card_comparison = compare_highest_card(\@pair1, \@pair2);

    if ($highest_card_comparison == 1) {
        return 1;
    } elsif ($highest_card_comparison == -1) {
        return -1;
    }

    my @singles1 = get_cards_occuring_n_times(\@l1, 1);
    my @singles2 = get_cards_occuring_n_times(\@l2, 1);

    my $highest_single_comparison = compare_highest_card(\@singles1, \@singles2);

    if ($highest_single_comparison == 1) {
        return 1;
    } elsif ($highest_single_comparison == -1) {
        return -1;
    }

    @singles1 = sort { $a->get_value <=> $b->get_value } @singles1;
    @singles2 = sort { $a->get_value <=> $b->get_value } @singles2;

    my $highest_single_suit1 = $singles1[$#singles1]->get_suit;
    my $highest_single_suit2 = $singles2[$#singles2]->get_suit;

    if ($singles1[0]->get_value == 0) {
        $highest_single_suit1 = $singles1[0]->get_suit;
    }

    if ($singles2[0]->get_value == 0) {
        $highest_single_suit2 = $singles2[0]->get_suit;
    }

    if ($highest_single_suit1 < $highest_single_suit2) {
        return 1;
    }

    return -1;
}

# =============== Helpers ===============

sub compare_highest_card {
    my ($l1_ref, $l2_ref) = @_;

    my @l1 = @$l1_ref;
    my @l2 = @$l2_ref;

    my @value_list1 = ();
    my @value_list2 = ();


    for my $i (0..$#l1) {
        push @value_list1, $l1[$i]->get_value;

        if ($l1[$i]->get_value == 0) {
            @value_list1[$i] = 13;

            if (@l1 == $Hand::HAND_SIZE) {
                if (HandIdentifier::is_straight(@l1) and !HandIdentifier::is_royal_straight(@l1)) {
                    @value_list1[$i] = 0;
                }
            }
        }

        push @value_list2, $l2[$i]->get_value;

        if ($l2[$i]->get_value == 0) {
            @value_list2[$i] = 13;

            if (@l2 == $Hand::HAND_SIZE) {
                if (HandIdentifier::is_straight(@l2) and !HandIdentifier::is_royal_straight(@l2)) {
                    @value_list2[$i] = 0;
                }
            }
        }
    }

    @value_list1 = sort { $b <=> $a } @value_list1;
    @value_list2 = sort { $b <=> $a } @value_list2;

    for my $i (0..$#value_list1) {
        if ($value_list1[$i] < $value_list2[$i]) {
            return 1;
        } elsif ($value_list1[$i] > $value_list2[$i]) {
            return -1;
        }
    }

    return 0;
}

sub get_cards_occuring_n_times {
    my ($card_list_ref, $n) = @_;

    my @card_list = @$card_list_ref;
    my @result = ();

    foreach my $card (@card_list) {
        if (!(grep {$_->get_value == $card->get_value} @result)) {
            if (HandIdentifier::get_card_count($card, @card_list) == $n) {
                push @result, $card;
            }
        }
    }

    return @result;
}

return 1;
