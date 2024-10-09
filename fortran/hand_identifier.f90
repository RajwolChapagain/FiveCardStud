module hand_identifier_module
    use card_module
    use hand_module
    implicit none
contains
    subroutine assign_type(h)
        type(hand) :: h
        type(card) :: cards(0:4)

        cards = h%get_sorted_cards()

        if (is_royal_straight_flush(cards)) then
            h%hand_type = 9
        else if (is_straight_flush(cards)) then
            h%hand_type = 8
        else if (is_flush(cards)) then
            h%hand_type = 5
        else if (is_straight(cards)) then
            h%hand_type = 4
        else
            h%hand_type = 0
        end if
    end subroutine assign_type

    logical function is_royal_straight_flush(cards) result(b)
        type(card) :: cards(0:4)

        b = .false.

        if (is_royal_straight(cards) .and. is_flush(cards)) then
            b = .true.
        end if
    end function is_royal_straight_flush

    logical function is_royal_straight(cards) result(b)
        type(card) :: cards(0:4)

        b = .false.

        if (cards(0)%get_value() == 0 .and. cards(1)%get_value() == 9 .and. cards(2)%get_value() == 10 .and. cards(3)%get_value() == 11 .and. cards(4)%get_value() == 12) then
            b = .true.
        end if
    end function is_royal_straight

    logical function is_straight_flush(cards) result(b)
        type(card) :: cards(0:4)

        b = .false.

        if (is_straight(cards) .and. is_flush(cards)) then
            b = .true.
        end if
    end function is_straight_flush
    
    logical function is_flush(cards) result(b)
        type(card) :: cards(0:4)
        integer :: prev_suit, i

        prev_suit = cards(0)%get_suit()
        b = .true.

        do i = 1, 4
            if (cards(i)%get_suit() /= prev_suit) then
                b = .false.
                return
            end if
            prev_suit = cards(i)%get_suit()
        end do
    end function is_flush

    logical function is_straight(cards) result(b)
        type(card) :: cards(0:4)
        integer :: prev_value, i

        b = .true.

        if (is_royal_straight(cards)) then
            return
        endif

        prev_value = cards(0)%get_value()

        do i = 1, 4
            if (cards(i)%get_value() /= prev_value + 1) then
                b = .false.
                return
            end if
            prev_value = cards(i)%get_value()
        end do
    end function is_straight
end module hand_identifier_module
