program pythagoras_llm
    implicit none
    character(len=200) :: query
    character(len=1) :: begin_char, end_char
    integer :: i, a, b, sinv, cosv, tanv, remainder, name_freq, cycle, div
    integer, parameter :: COUNTRY_LEN = 3! "NGA"
    integer :: matrix(3,3)
    character(len=20) :: prediction

    print *, "Enter search query:"
    read(*,'(A)') query

    call process_query(trim(query))

contains

    function ascii_val(c) result(val)
        character(len=1), intent(in) :: c
        integer :: val
        val = iachar(c)
    end function

    function name_freq(str) result(freq)
        character(len=*), intent(in) :: str
        integer :: freq, j
        freq = 0
        do j=1,len_trim(str)
            if(iachar(str(j:j)) >= 65) then
                freq = freq + iachar(str(j:j))
            end if
        end do
        freq = mod(freq, 100)
    end function

    subroutine pythagoras_mod(a,b,sinv,cosv,tanv)
        integer, intent(in) :: a,b
        integer, intent(out) :: sinv,cosv,tanv
        if(b==0) then
            sinv=0; cosv=1; tanv=1; return
        end if
        sinv = mod(a,b)
        if(a==0) then
            cosv=1
        else
            cosv = mod(b,a)
        end if
        if(cosv==0) then
            tanv=1
        else
            tanv = mod(a,cosv)
        end if
    end subroutine

    subroutine process_query(q)
        character(len=*), intent(in) :: q
        character(len=20), dimension(20) :: words
        integer :: word_count, j, k

       ! naive split by space
        word_count = 0
        j = 1
        do while(j <= len_trim(q))
            if(q(j:j) /= ' ') then
                word_count = word_count + 1
                k = j
                do while(k <= len_trim(q).and. q(k:k) /= ' ')
                    k = k + 1
                end do
                words(word_count) = q(j:k-1)
                j = k
            else
                j = j + 1
            end if
        end do

        remainder = 0
        print *, "Input: ", trim(q)

       ! process every 2 words
        do i=1, word_count-1, 2
            a = ascii_val(words(i)(1:1))
            b = ascii_val(words(i+1)(1:1))
            call pythagoras_mod(a,b,sinv,cosv,tanv)
            print *, "Chunk: ", trim(words(i)), trim(words(i+1)), " sin=",sinv," cos=",cosv," tan=",tanv

            name_freq = name_freq(words(i)//words(i+1))
            matrix(1,1)=sinv; matrix(1,2)=cosv; matrix(1,3)=name_freq
            matrix(2,1)=tanv; matrix(2,2)=COUNTRY_LEN; matrix(2,3)=sinv
            matrix(3,1)=name_freq; matrix(3,2)=cosv; matrix(3,3)=COUNTRY_LEN

            remainder = mod(remainder + sinv + cosv + tanv, 97)
        end do

        begin_char = q(1:1)
        end_char = q(len_trim(q):len_trim(q))
        remainder = mod(remainder + mod(ascii_val(begin_char), ascii_val(end_char)), 97)
        print *, "Begin%End: ", mod(ascii_val(begin_char), ascii_val(end_char))

        prediction = ""
        cycle = 0
        do while(remainder /= 0.and. cycle < 10)
            div = cycle + 2
            remainder = mod(remainder, div)
            prediction = trim(prediction)//achar(97 + mod(remainder,26))
            cycle = cycle + 1
        end do
        print *, "Prediction: ", trim(prediction)
    end subroutine

end program pythagoras_llm