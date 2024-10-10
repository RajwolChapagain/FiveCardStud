module hand_sorter_module
    use card_module
    use hand_module

    implicit none

contains
    subroutine sort_hands(hands)
        type(hand) :: hands(0:5)

        call sort_by_type(hands)
    end subroutine sort_hands

    subroutine sort_by_type(hands)
        type(hand) :: hands(0:5)
        
        call sort(hands, compare_hands_by_type)
    end subroutine sort_by_type

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
        type(hand) :: hand1, hand2

        b = .false.

        if (hand1%hand_type < hand2%hand_type) then
            b = .true.
        end if
    end function compare_hands_by_type


end module
