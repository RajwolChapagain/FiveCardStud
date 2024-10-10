module hand_sorter_module
    use card_module
    use hand_module

    implicit none

contains
    subroutine sort_hands(hands)
        type(hand) :: hands(0:5)

        call sort_by_type(hands)
        call sort_ties(hands)
    end subroutine sort_hands

    subroutine sort_by_type(hands)
        type(hand) :: hands(0:5)
        
        call sort(hands, compare_hands_by_type)
    end subroutine sort_by_type

    subroutine sort_ties(hands)
        type(hand) :: hands(0:5)
        integer :: start_index, last_type, i

        start_index = 0
        last_type = hands(0)%hand_type

        do i = 0, 5
            if (hands(i)%hand_type /= last_type) then
                call sort_subarray(hands(start_index: i-1))

                start_index = i
                last_type = hands(i)%hand_type
            else if (i == size(hands) - 1) then
                call sort_subarray(hands(start_index:))
            end if
        end do
    end subroutine sort_ties

    subroutine sort_subarray(hands)
        type(hand) :: hands(:)
        integer :: hand_type

        hand_type = hands(1)%hand_type

        if (hand_type == 9) then
            call sort(hands, compare_royal_flush)
        end if

    end subroutine sort_subarray
    
    subroutine sort(array, cmp_fn)
        type(hand) :: array(:)
        type(hand) :: temp
        integer :: i, j
        logical :: cmp_fn

        do i = 1, size(array) - 1
            do j = 1, size(array) - i
                if (cmp_fn(array(j), array(j + 1))) then
                    ! Swap the sorted_cards
                    temp = array(j)
                    array(j) = array(j + 1)
                    array(j + 1) = temp
                end if
            end do
        end do
    end subroutine sort

    !=============== logical comparator methods ===============
    ! All return true if hand1 is weaker

    logical function compare_hands_by_type(hand1, hand2) result(b)
        type(hand), intent(in) :: hand1, hand2

        b = .false.

        if (hand1%hand_type < hand2%hand_type) then
            b = .true.
        end if
    end function compare_hands_by_type


    logical function compare_royal_flush(h1, h2) result(b)
        type(hand), intent(in) :: h1, h2
        type(card) :: l1(5), l2(5)

        b = .true.

        l1 = h1%get_sorted_cards()
        l2 = h2%get_sorted_cards()

        if (l1(1)%get_suit() > l2(1)%get_suit()) then
            b = .false.
         end if
    end function compare_royal_flush

end module
