program pythagoras_llm
    implicit none
    character(len=200) :: query
    character(len=3), parameter :: COUNTRY = "NGA"
    integer :: i, remainder, beginChar, endChar, a, b, sinv, cosv, tanv, nameFreq
    character(len=20), dimension(20) :: words
    integer :: wordCount

    query = "I going to the market"
    call split_words(query, words, wordCount)

    remainder = 0
    print *, "Input: ", trim(query)

    do i = 1, wordCount-1, 2
        a = ichar(words(i)(1:1))
        b = ichar(words(i+1)(1:1))

        sinv = mod(a, b)
        cosv = merge(mod(b, a), 1, a /= 0)
        tanv = merge(mod(a, cosv), 1, cosv /= 0)

        nameFreq = name_frequency(trim(words(i)) // trim(words(i+1)))

        print *, "Chunk: ", trim(words(i)), trim(words(i+1)), " sin=", sinv, " cos=", cosv, " tan=", tanv
        remainder = mod(remainder + sinv + cosv + tanv, 97)
    end do

    beginChar = ichar(query(1:1))
    endChar = ichar(query(len_trim(query):len_trim(query)))
    remainder = mod(remainder + mod(beginChar, endChar), 97)
    print *, "Begin%End: ", mod(beginChar, endChar)

    call prediction_loop(remainder)

contains
    subroutine split_words(str, arr, count)
        character(len=*), intent(in) :: str
        character(len=20), intent(out) :: arr(:)
        integer, intent(out) :: count
        integer :: i, j
        count = 0
        j = 1
        do i = 1, len_trim(str)
            if (str(i:i) == ' ') then
                count = count + 1
                j = i + 1
            end if
        end do
        count = count + 1
        read(str, *) arr(1:count)
    end subroutine

    integer function name_frequency(s)
        character(len=*), intent(in) :: s
        integer :: i, sum
        sum = 0
        do i = 1, len_trim(s)
            if (s(i:i) >= 'A'.and. s(i:i) <= 'Z') then
                sum = sum + ichar(s(i:i))
            else if (s(i:i) >= 'a'.and. s(i:i) <= 'z') then
                sum = sum + ichar(s(i:i))
            end if
        end do
        name_frequency = mod(sum, 100)
    end function

    subroutine prediction_loop(rem)
        integer, intent(inout) :: rem
        integer :: cycle, div
        character :: c
        cycle = 0
        do while (rem /= 0.and. cycle < 10)
            div = cycle + 2
            rem = mod(rem, div)
            c = char(97 + mod(rem, 26))
            print *, "Cycle ", cycle+1, ": rem=", rem, " char=", c
            cycle = cycle + 1
        end do
    end subroutine
end program