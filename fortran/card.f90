module card_module
    implicit none

    private
    public :: Card, init_card, get_value, get_suit
    public :: VALUE_MAP!, SUIT_MAP

    character(2) :: VALUE_MAP(13)
    character(1) :: SUIT_MAP(4)
    data VALUE_MAP / 'A', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K' /
    data SUIT_MAP / 'D', 'C', 'H', 'S' /

    type Card
        integer :: val, suit
    contains
        procedure :: init_card, get_value, get_suit
    end type Card

contains
    subroutine init_card(this, val, suit)
        class(Card) :: this
        integer :: val, suit
        this%val = val
        this%suit = suit
    end subroutine init_card

    function get_value(this) result (val)
        class(Card) :: this
        integer :: val
        val = this%val
    end function get_value

    function get_suit(this) result (suit)
        class(Card) :: this
        integer :: suit
        suit = this%suit
    end function get_suit

end module card_module
