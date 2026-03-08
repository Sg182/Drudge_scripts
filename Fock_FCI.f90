!==========================================================
! Convert integer N to binary occupation vector BIN(:)
!==========================================================
Module Num_Bin
Implicit None
Contains


Subroutine NumToBinary(Num, BINARY, Nbits)
    Implicit None
    Integer, Intent(In):: Num,Nbits
    Integer, Allocatable, Intent(out):: Binary(:)
    Integer :: I,TMP

    Allocate(BINARY(Nbits))
    
    TMP = Num

    Do I = 1,Nbits
        BINARY(I) = Mod(TMP, 2)
        TMP    = TMP / 2
    End Do
End Subroutine NumToBinary


!==========================================================
! Convert binary occupation vector BIN(:) to integer N
!==========================================================
Subroutine BinaryToNum(BINARY, N)
    Implicit None
    Integer, Intent(In)  :: BINARY(:)
    !Integer, Intent(In)  :: NBITS
    Integer, Intent(Out) :: N
    Integer              :: I, POWER,NBITS

    N = 0

    NBITS = Size(BINARY)
    POWER = 1
    Do I = 1, NBITS
        N = N + BINARY(I) * POWER
        POWER = POWER * 2
    End Do
End Subroutine BinaryToNum

End Module