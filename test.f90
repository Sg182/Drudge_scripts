Program TestBinary
    Use Num_Bin
    Implicit None

    Integer, Parameter :: NBITS = 6
    Integer, Allocatable :: BIN(:)
    Integer :: N, Nback
    Integer, Dimension(8) :: testvals
    Integer :: i

    ! some numbers to test
    testvals = [0, 1, 2, 20, 5, 13, 27, 63]

    Print *, 'Testing binary conversion for NBITS =', NBITS
    Print *, '---------------------------------------'

    Do I = 1, Size(testvals)
        N = testvals(i)

        ! convert N -> BIN
        Call NumToBinary(N, BIN, NBITS)

        ! convert BIN -> Nback
        Call BinaryToNum(BIN, Nback)

        Write(*,'(A,I3,A,6(I1,1X),A,I3)') 'N = ', N, '  BIN = ', BIN, '  back = ', Nback

        Deallocate(BIN)
    End Do

End Program TestBinary
