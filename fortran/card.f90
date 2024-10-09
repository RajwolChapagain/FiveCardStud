module card_module
    implicit none

    private
    public :: VALUE_MAP, SUIT_MAP, Card, get_value_index, get_suit_index

    character(2) :: VALUE_MAP(0:12)
    character(1) :: SUIT_MAP(0:3)
    data VALUE_MAP / 'A', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K' /
    data SUIT_MAP / 'D', 'C', 'H', 'S' /

    type Card
        integer :: val, suit
    contains
        procedure :: init_card, init_card_from_string, get_value, get_suit, to_string
    end type Card

contains
    subroutine init_card(this, val, suit)
        class(Card) :: this
        integer :: val, suit
        this%val = val
        this%suit = suit
    end subroutine init_card

    ! Expects exactly 3-character wide string: ' AH', '10D', etc
    subroutine init_card_from_string(this, string)
        class(Card) :: this
        character(3), intent(in) :: string
        character(3) :: result_string
        character(:), allocatable :: value_string
        character(1) :: suit_string

        result_string = adjustl(string)

        if (len_trim(result_string) == 2) then
            value_string = result_string(1:1)
            suit_string = result_string(2:2)
        else !Card value is 10
            value_string = result_string(1:2)
            suit_string = result_string(3:3)
        end if

        call this%init_card(get_value_index(value_string), get_suit_index(suit_string))
    end subroutine init_card_from_string

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

    function to_string(this) result (padded_string)
        class(card) :: this
        character(:), allocatable :: string
        character(4) :: padded_string
        string = trim(VALUE_MAP(this%get_value())) // trim(SUIT_MAP(this%get_suit()))
        padded_string = adjustl(string)
    end function to_string

    function get_value_index(value_string) result (ind)
        character(*) :: value_string
        integer :: i, ind

        ind = -1
        value_map_loop : do i = 0, size(VALUE_MAP) - 1
            if (VALUE_MAP(i) == value_string) then
                ind = i
                exit value_map_loop
            end if
        end do value_map_loop
    end function get_value_index

    function get_suit_index(suit_string) result(ind)
        character(1) :: suit_string
        integer :: i, ind

        ind = -1
        suit_map_loop : do i = 0, size(SUIT_MAP) - 1
            if (SUIT_MAP(i) == suit_string) then
                ind = i
                exit suit_map_loop
            end if
        end do suit_map_loop

    end function get_suit_index

end module card_module
