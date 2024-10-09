program main
    use card_module

    implicit none
    type(Card) :: deck(0:51)

    print *, '*** P O K E R   H A N D   A N A L Y Z E R ***'
    print *, ''
    print *, ''
    
    call init_deck(deck)
    call print_deck(deck)


contains
    subroutine init_deck(deck)
        type(Card) :: deck(0:51)
        integer :: i
        real :: rand
        integer :: random_int
        type(Card) :: tmp

        do i = 0, size(deck) -1
            call deck(i)%init_card(mod(i, 13), i / 13)
        end do

        do i = 0, size(deck) - 1
            call random_number(rand)
            random_int = int(rand * 52)
            tmp = deck(i)
            deck(i) = deck(random_int)
            deck(random_int) = tmp
        end do
    end subroutine init_deck

    subroutine print_deck(deck)
        type(Card) :: deck(0:51)
        integer :: i
        character(:), allocatable :: line

        print *, '*** USING RANDOMIZED DECK OF CARDS ***'
        print *, ''
        print *, '*** Shuffled 52 card deck:'

        line = ''
        do i = 0, size(deck) - 1
            line = line // deck(i)%to_string()
            if (mod(i+1, 13) == 0) then
                write(*, '(A)') trim(line)
                line = ''
            end if 
        end do
    end subroutine print_deck
    
end program main
