program main
    ! Main program calls the functions and subroutines
    integer :: i
    integer, dimension(100) :: data_array
    integer, dimension(:), allocatable :: primes_array
    integer, dimension(:), allocatable :: goldbach_array
    integer, dimension(:), allocatable :: goldbach_array_2
    logical :: file_exists

    ! Calling the read function to read data.txt
    ! Collecting the numbers from the file and putting them into an array
    data_array = read()

    inquire(file = 'results.txt', exist = file_exists) ! Checking if results file already exists

    if (file_exists) then ! If so, then delete results file, will make new one in the write subroutine
        open(1,file='results.txt', status = 'old')
        close(1, status = 'delete')
    end if
    
    ! Do Loop, looping through the array to call the primes function, goldbach and write subroutines
    do i=1, size(data_array)
        primes_array = primes(data_array(i))  
        call goldbach(data_array(i), primes_array, goldbach_array, goldbach_array_2)
        call write(goldbach_array, goldbach_array_2, data_array(i))
        deallocate(primes_array) ! Deallocate the arrays so on next iteration, arrays can be reused with new numbers
        deallocate(goldbach_array)
        deallocate(goldbach_array_2)
        write(*,*) i, data_array(i) ! Prints out to the terminal on what iteration it is on
    end do

contains
function primes(num_input) result(primes_array)
    ! This function finds all the prime numbers within the number from the file
    ! It returns an array with prime numbers
    implicit none 

    integer :: count, num_input, number, divisor, i
    integer, dimension(10000) :: count_array ! This array is needed to catch all the primes found
    
    integer, dimension(:), allocatable :: primes_array
    ! We will allocate this array depending on the number of primes found
    ! and then copy the numbers from count_array

    ! Set all the elements in this array to zero
    do i = 1, size(count_array)
        count_array(i) = 0
    end do
    
    count = 1
    count_array(count) = 2 ! Insert the first prime number manually

    do number = 3, num_input, 2
        divisor = 3
        do
            if (divisor*divisor > number .or. mod(number, divisor) == 0) exit
            divisor = divisor + 2 
        end do

        if (divisor*divisor > number) then
            count_array(count+1) = number ! Add the prime number into the array at the correct index
            count = count + 1
        end if
    end do

    do i = 1, size(count_array)
        if (count_array(i) == 0) exit ! Iterate through this array until the element is zero
    end do

    i = i - 1 ! Set our index properly so we can allocate the array

    allocate(primes_array(i)) ! Allocates the primes array by using the number of primes found

    do i = 1, size(primes_array)
        primes_array(i) = count_array(i) ! Copy the values over to the new array
        !write(*,*) primes_array(i)
    end do
end function primes

function read () result (data_array)
    ! This function will open the 'data.txt' file and read from it
    implicit none 

    integer, parameter :: list=100
    integer :: dat(list), i
    integer, dimension(100) :: data_array ! Use an array to store the numbers found
   
    open(1, file='data.txt')
    do i=1, list
        read(1,*) dat(i)
        data_array(i) = dat(i)
    end do
    close(1)
end function read

subroutine goldbach (data_array, primes_array, goldbach_array, goldbach_array_2)
    ! This subroutine will compute the goldbach primes and also write to the 'results1.txt' file
    ! Will modify its arguments instead of returning a value
    implicit none

    integer, dimension (10000) :: count_array
    integer, dimension (10000) :: count_array_2
    integer, dimension (:), allocatable :: goldbach_array
    integer, dimension (:), allocatable :: goldbach_array_2
    integer, dimension (:) ::  primes_array
    integer :: count, i, q, data_array

    count = 0
    count_array = 0
    count_array_2 = 0

    if (data_array >= 4 .and. mod(data_array, 2) == 0) then ! Finds the goldbach pairs and adds them to the arrays
        do i = 1, size(primes_array)
            if (primes_array(i) > (data_array / 2)) exit
            q = data_array - primes_array(i)
            if (ANY(primes_array == q)) then
                count_array(count+1) = q
                count_array_2(count+1) = primes_array(i)
                count = count + 1
            end if
        end do
    end if

    do i = 1, size(count_array)
        if (count_array(i) == 0) exit ! Iterate through this array until the element is zero
    end do

    i = i - 1 ! Set our index properly so we can allocate the array

    allocate(goldbach_array(i)) ! Allocates the goldbach array by using the number of values found in count array

    do i = 1, size(goldbach_array)
        goldbach_array(i) = count_array(i) ! Copy the values over to the new array
    end do

    do i = 1, size(count_array_2)
        if (count_array_2(i) == 0) exit ! Iterate through this array until the element is zero
    end do

    i = i - 1 ! Set our index properly so we can allocate the array

    allocate(goldbach_array_2(i)) ! Allocates the goldbach array by using the number of values found in count array

    do i = 1, size(goldbach_array_2)
        goldbach_array_2(i) = count_array_2(i) ! Copy the values over to the new array
    end do
end subroutine goldbach

subroutine write(goldbach_array, goldbach_array_2, data_array)
    implicit none

    integer, dimension (:) ::  goldbach_array, goldbach_array_2
    integer :: data_array, i

    open(1,file='results.txt', action = 'write', position = 'append') ! Open / Creates a text file
    write(1, '(A, I0, A, I0, A)') 'We found ', size(goldbach_array), ' Goldbach pair(s) for ', data_array, '.' ! First write the header for the number before writing the goldbachs
    
    do i = 1, size(goldbach_array)
        if (goldbach_array(i) == 0) exit
        write(1,'(I0, A, I0, A, I0)') data_array, ' = ', goldbach_array_2(i), ' + ', goldbach_array(i) ! Writes to file
    end do

    write(1,*) ! Writes an empty line to file
    close(1) 
end subroutine write

end program main