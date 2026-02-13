    Module Broyden_SFS_CCSDT
! Implementation of modified Broyden's methods
! Ref: PRC 78, 014318; PRB, 38, 12807 
    Use Precision
    Use Constants
    Use CCRes_SFS
    Implicit None
    Private
    Public  :: BroydenIterTCC,Mat2Vec, Vec2Mat, BroydenStep, UpdateBroyden,ShutDownBroyden, SetUpBroyden, EvalF
! nBrd: # of unknowns, pBrd: # of Broyden step stored
    Integer                        :: nBrd, pBrd
! Local copies of PUCC variables   
    Integer                        :: nsBrd, ndBrd, ntBrd, nqBrd
    Integer                        :: NAOBrd
    Complex(Kind=pr)               :: H000Brd
    Complex(Kind=pr), Allocatable  :: H010Brd(:), H001Brd(:), H100Brd(:)
    Complex(Kind=pr), Allocatable  :: H020Brd(:,:), H101Brd(:,:)
    Complex(Kind=pr), Allocatable  :: H200Brd(:,:), H002Brd(:,:)
    Complex(Kind=pr), Allocatable  :: H110Brd(:,:), H011Brd(:,:)
    Complex(Kind=pr), Allocatable  :: H030Brd(:,:,:), H111Brd(:,:,:), H120Brd(:,:,:), H210Brd(:,:,:)
    Complex(Kind=pr), Allocatable  :: H021Brd(:,:,:), H201Brd(:,:,:), H102Brd(:,:,:), H012Brd(:,:,:)
    Complex(Kind=pr), Allocatable  :: H003Brd(:,:,:), H300Brd(:,:,:)
    Complex(Kind=pr), Allocatable  :: UBrd(:), VBrd(:)
    Complex(Kind=pr), Allocatable  :: MixVec(:)
    Complex(Kind=pr)               :: EneBrd
    Logical                        :: DoCCD = .false.
    Logical                        :: DoCCSD = .false.

    Contains

    Subroutine BroydenIterTCC(Ene,T1,T2,T3,BCSU,BCSV,NAO,pBrdIn,DoCCDIn, &
                              H000,H010,H100,H001,H020,H101,H200,H002,H011,H110,H030,H111,H210,H120, &
                              H012,H021,H201,H102,H300,H003)!,naobrd,nbrd) ! I made changes here included naobrd,nbrd
    Implicit None
    Integer,           Intent(In)    :: NAO, pBrdIn! ,naobrd,nbrd ! added naobrd, nbrd
    Complex (Kind=pr), Intent(Out)   :: Ene
    Real(Kind=pr) :: damp

    Complex (Kind=pr), Intent(InOut) :: T1(NAO)
    Complex (Kind=pr), Intent(InOut) :: T2(NAO,NAO)
    Complex (Kind=pr), Intent(InOut) :: T3(NAO,NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: BCSU(NAO), BCSV(NAO)
    Complex (Kind=pr), Intent(in)    :: H000, H010(NAO), H100(NAO), H001(NAO)
    Complex (Kind=pr), Intent(in)    :: H020(NAO,NAO), H101(NAO,NAO)
    Complex (Kind=pr), Intent(in)    :: H200(NAO,NAO), H002(NAO,NAO)
    Complex (Kind=pr), Intent(in)    :: H110(NAO,NAO), H011(NAO,NAO)
    Complex (Kind=pr), Intent(in)    :: H030(NAO,NAO,NAO),H111(NAO,NAO,NAO)
    Complex (Kind=pr), Intent(in)    :: H120(NAO,NAO,NAO), H210(NAO,NAO,NAO)
    Complex (Kind=pr), Intent(in)    :: H021(NAO,NAO,NAO),H012(NAO,NAO,NAO)
    Complex (Kind=pr), Intent(in)    :: H201(NAO,NAO,NAO), H102(NAO,NAO,NAO)
    Complex (Kind=pr), Intent(in)    :: H300(NAO,NAO,NAO), H003(NAO,NAO,NAO)

    Logical,           Intent(In)    :: DoCCDIn
    Integer                          :: I, J, K, L 
! Broyden iteration stuff
    Complex (Kind=pr), Allocatable   :: xold(:), xnew(:)
    Complex (Kind=pr), Allocatable   :: fold(:), fnew(:)
    Complex (Kind=pr), Allocatable   :: dF(:,:), dx(:,:)
    Complex (Kind=pr)                :: Denom
    Real (Kind=pr),    Allocatable   :: w(:)
    Real (Kind=pr)                   :: w0, mix
    Real (Kind=pr),    Parameter     :: TolMax = 1.0E-10_pr
    Integer,           Parameter     :: CycMax = 2000
    Integer                          :: NIter, IAlloc
    Real (Kind=pr)                   :: dT, dTold 
    Real (Kind=pr)                   :: ResNew, ResOld 
! Set things up 
    DoCCD = DoCCDIn
    DoCCD = .FALSE.
    DoCCSD = .FALSE.

    IF (DoCCD) Write(*,*) "Enter Broyden CCD"
 ! y contains T1 T2
    NAOBrd = NAO
    nsBrd = NAO                 !Made changes here
    ndBrd = NAO*(NAO-1)/2
    ntBrd = NAO*(NAO-1)*(NAO-2)/6       !
    nBrd  = nsBrd + ndBrd + ntBrd      !
    
    
   ! Write(*,*) "After SetUp: NAOBrd = ", NAOBrd, " nBrd = ", nBrd

    Call SetUpBroyden(BCSU,BCSV,NAO,H000,H010,H100,H001,H020,H101,H200,H002,H110,H011,H030,H111,H120, &
              H210,H021,H012,H201,H102,H300,H003)
    pBrd = pBrdIn
    
    print*, "T1 at the beginning is: ", maxval(real(T1))
    print*, "T2 at the beginning is: ", maxval(real(T2))
    print*, "T3 at the beginning is: ", maxval(real(T3)) 
! Allocate
    Allocate(xold(nBrd), xnew(nBrd), fold(nBrd), fnew(nBrd),    &
             Stat=IAlloc)
    If(IAlloc/=0) Stop "Could not allocate in BroydenIter"
    If(pBrd>0) then
      Allocate(dF(nBrd,pBrd), dx(nBrd,pBrd), w(pBrd),             &
               Stat=IAlloc)
      If(IAlloc/=0) Stop "Could not allocate in BroydenIter"
    EndIf
   Write(*,*) "Entered SFS-BCS CCSDT"
    Open(8,File='Output',Position='Append')
    Write(8,1020)
    Write(8,1010)
    Write(8,1020)
    Write(8,1030)
! Initialize 
    If(DoCCD) T1 = Zero ! CCD!
    If(DoCCSD) T3 = Zero !CCSD
  
    Call Mat2Vec(xold,T1,T2,T3,NAOBrd,nBrd)
    NIter = 0
    dT = 1.0_pr
    dF = Zero
    dx = Zero 
    w  = One
    w0 = 0.01_pr
    mix= 0.80_pr
    Call EvalF(xold,fold,NAOBrd,nBrd)
    ResOld = Sqrt(Dot_Product(fold,fold))
    ResNew = ResOld
    Write(8,1040) Real(EneBrd),NIter,dT
    print*,"H101 ", real(maxval(real(H101)))
    print*,"H020 ", real(maxval(real(H020)))
    print*,"H010 ", real(minval(real(H010)))
    print*,"H111 ", real(maxval(real(H111)))
    print*,"H200 ", real(maxval(real(H200)))
    print*,"H011 ", real(maxval(real(H011)))
    print*,"H210 ", real(maxval(real(H210)))
    print*,"H021 ", real(maxval(real(H021)))
    print*,"H012 ", real(maxval(real(H012)))
    print*,"H102 ", real(maxval(real(H102)))
    print*,"H201 ", real(maxval(real(H201)))
    print*,"H300 ", real(maxval(real(H300)))
    print*,"H030 ", real(maxval(real(H030)))
    print*,"H003 ", real(maxval(real(H003)))
    print*,"H100 ", real(maxval(real(H100)))
    print*,"H001 ", real(maxval(real(H001)))
    print*,"H002 ", real(maxval(real(H002)))
    print*,"H000 ", real(H000)
! Build the mixing vector (Approximate -J^-1)
    Do I = 1, NAO
      Denom = H101(I,I) + Two*H010(I) + Four*H020(I,I)
      call floor_complex(Denom, 0.30_pr)
      print *, "min|D1|=", (abs(Denom)), "  max|D1|=", (abs(Denom)) 
      T1(I) = -One/Denom 
    Do J = 1, NAO
      Denom = (H101(I,I)+H101(J,J)) + Two*(H010(I)+H010(J)) &
            + Four*(H020(I,I)+H020(J,J)+Two*H020(I,J))
      print *, "min|D2|=", (abs(Denom)), "  max|D2|=", (abs(Denom))
      call floor_complex(Denom, 0.30_pr) 
      T2(I,J) = -One/Denom 
    EndDo
    EndDo

    Do K = 3, NAO
    Do J = 2, K-1
    Do I = 1, J-1
      Denom = (H101(I,I)+H101(J,J)+H101(K,K))                    &
            + Two*(H010(I)+H010(J)+H010(K))                      &
            + Four*( (H020(I,I)+H020(J,J)+H020(K,K))             &
                   + Two*(H020(I,J)+H020(I,K)+H020(J,K)) )
      call floor_complex(Denom, 0.60_pr)   ! stronger floor often helps for T3
      T3(I,J,K) = -One/Denom

      ! If you store the full symmetric tensor, you can fill permutations:
      ! T3(I,K,J) = T3(I,J,K)
      ! T3(J,I,K) = T3(I,J,K)
      ! T3(J,K,I) = T3(I,J,K)
      ! T3(K,I,J) = T3(I,J,K)
      ! T3(K,J,I) = T3(I,J,K)
    EndDo
    EndDo
    EndDo

    

   

    Call Mat2Vec(MixVec,T1,T2,T3,NAOBrd,nBrd)
! Start iteration
    xnew = xold
    
    Do While(dT >= TolMax)
      NIter = NIter + 1
      
!     Write(*,*) "NIter", NIter
! Perform the Broyden step
 !     Write(*,*) "nBrd = ", nBrd
      damp = 1.0_pr
      Call BroydenStep(xnew,xold,fold,mix,w0,w,dF,dx,NIter,nBrd,pBrd,damp)
      Call EvalF(xnew,fnew,NAOBrd,nBrd)

      ! Print every 10 iterations to avoid huge logs; change 10 -> 1 to print every iter
      If (mod(NIter,10)==1 .or. NIter<=3) then
      Call PrintAmpsResFromVec(8, xnew, fnew, NAOBrd, nBrd, nsBrd, ndBrd, ntBrd)
      End If

      ResOld = ResNew
      ResNew = Sqrt(Dot_Product(fnew,fnew))
! Check convergence
      dTold = dT
      dT = MaxVal(Abs(fnew))
! Update dF and dx matrices
     ! Write(*,*) "nBrd = ", nBrd
      Call UpdateBroyden(dF,dx,fnew,fold,xnew,xold,nBrd, pBrd)
      Write(8,1040) Real(EneBrd),NIter,dT
      
      If (NIter > CycMax) then
        Call Vec2Mat(xnew,T1,T2,T3,NAOBrd,nBrd)
! checkpoint T1 and T2
        
        Open(70,File="Chk_T1",status="Replace")
        Write(70,*) T1
        Close(70)
        Open(70,File="Chk_T2",status="Replace")
        Write(70,*) T2
        Close(70)
        Open(70,File="Chk_T3",status="Replace")
        Write(70,*) T3
        close(70)
        Stop 'Too many cycles in PbarHbar Feedback loop'
      EndIf
! Prepare for the next step
      xold = xnew
      fold = fnew
      flush(6)
      flush(8)
! Debug: save T1 T2 at each iteration
     ! Write(*,*) "Size of T1 = ", Size(T1)
     ! Write(*,*) "Size of T2 = ", Size(T2,1), Size(T2,2)
     ! Write(*,*) "NAOBrd = ", NAOBrd
     ! Write(*,*) "nBrd = ", nBrd

      Call Vec2Mat(xnew,T1,T2,T3,NAOBrd,nBrd)
      Open(70,File="Chk_T1",status="Replace")
      Write(70,*) T1
      Close(70)
      Open(70,File="Chk_T2",status="Replace")
      Write(70,*) T2
      Close(70)
      Open(70,File="Chk_T3",status="Replace")
      Write(70,*) T3
      close(70)
! End of Debug
    EndDo
  

    Call Vec2Mat(xnew,T1,T2,T3,NAOBrd,nBrd)
    Ene = EneBrd
    Write(8,1020)
    Write(8,1050) NIter
    Write(8,1070) Real(EneBrd)
    Write(8,1080) Aimag(EneBrd)
    Write(8,1000)
    Close(8)


        !================= === FINAL RESIDUAL CHECK (authoritative) ==================================================================
 BLOCK
Complex(Kind=pr), Allocatable :: fx_final(:)
Complex(Kind=pr), Allocatable :: R1fin(:), R2fin(:,:), R3fin(:,:,:)
Real(Kind=pr) :: maxAll, maxR1, maxR2, maxR3


Allocate(fx_final(nBrd), R1fin(NAOBrd), R2fin(NAOBrd,NAOBrd), R3fin(NAOBrd,NAOBrd,NAOBrd), Stat=IAlloc)
If (IAlloc/=0) Stop "Could not allocate in final residual check"

! Evaluate F at the *final* amplitudes (xnew is already the final)
Call Mat2Vec(xold,T1,T2,T3,NAOBrd,nBrd)    ! pack current T back to xold
Call EvalF(xold, fx_final, NAOBrd, nBrd)   ! compute fresh residuals

Call Vec2Mat(fx_final, R1fin, R2fin, R3fin, NAOBrd, nBrd)

maxR1 = maxval(abs(R1fin))
! doubles packing i<j only:
maxR2 = 0.0_pr
Do j=2,NAOBrd
  Do i=1,j-1
    maxR2 = max(maxR2, abs(R2fin(i,j)))
  End Do
End Do

maxR3 = 0.0_pr
Do k=3,NAOBrd
  Do j=2,k-1
    Do i=1,j-1
      maxR3 = max(maxR3, abs(R3fin(i,j,k)))
    End Do
  End Do
End Do

maxAll = max( maxR1, maxR2, maxR3)

write(8,'(A,1PE12.4)') 'FINAL max|Res1| = ', maxR1
write(8,'(A,1PE12.4)') 'FINAL max|Res2| = ', maxR2
write(8,'(A,1PE12.4)') 'FINAL max|Res3| = ', maxR3
write(8,'(A,1PE12.4)') 'FINAL max|Res | = ', maxAll
Call PrintAmpsResFromVec(8, xnew, fnew, NAOBrd, nBrd, nsBrd, ndBrd, ntBrd)
! If not actually converged, flag it clearly:
If (maxAll > TolMax) Then
  write(8,'(A)') '*** WARNING: Residual > TolMax at exit; NOT converged by residual! ***'
End If

Deallocate(fx_final, R1fin, R2fin,R3fin, Stat=IAlloc)
If (IAlloc/=0) Stop "Could not deallocate in final residual check"
END BLOCK

!   Write(*,*) "UCCSD energy from Broyden:", EneBrd
! Outputs
1000  Format(14x,'**************************************************')
1010  Format(14X,'*          BCS CCSD summary follows              *')
1020  Format(14x,'*------------------------------------------------*')
1030  Format(14X,'*   CCSD Energy    Iteration    Biggest Res      *')
1040  Format(14X,'* ',F15.10,2x,I5,7X,F14.10,4x,'*')
1050  Format(14x,'*  UCC has converged in ',I3,' iterations',11x,'*')
1070  Format(14x,'*  Final UCC Energy is (real) ',F15.9,'a.u.*')
1080  Format(14x,'*  Final UCC Energy is (imag) ',F15.9,'a.u.*')
! deallocate
    Deallocate(xold, xnew, fold, fnew, Stat=IAlloc)
    If(pBrd>0) Deallocate(dF, dx, w, Stat=IAlloc)
    If(IAlloc/=0) Stop "Could not deallocate in BroydenIter"
    Call ShutDownBroyden
    Return
    End Subroutine BroydenIterTCC

    Subroutine SetUpBroyden(BCSU,BCSV,NAO, &
                            H000,H010,H100,H001,H020,H101,H200,H002,H110,H011,H030,H111,H120, &
                            H210,H021,H012,H201,H102,H300,H003)
    Implicit None
    Integer,           Intent(In)    :: NAO
    Complex (Kind=pr), Intent(In)    :: BCSU(NAO), BCSV(NAO)
    Complex (Kind=pr), Intent(in)   :: H000, H010(NAO), H100(NAO), H001(NAO)
    Complex (Kind=pr), Intent(in)   :: H020(NAO,NAO), H101(NAO,NAO)
    Complex (Kind=pr), Intent(in)   :: H200(NAO,NAO), H002(NAO,NAO)
    Complex (Kind=pr), Intent(in)   :: H110(NAO,NAO), H011(NAO,NAO)
    Complex (Kind=pr), Intent(in)   ::H030(NAO,NAO,NAO),H111(NAO,NAO,NAO)
    Complex (Kind=pr), Intent(in) :: H120(NAO,NAO,NAO), H210(NAO,NAO,NAO)
    Complex (Kind=pr), Intent(in) :: H021(NAO,NAO,NAO),H012(NAO,NAO,NAO)
    Complex (Kind=pr), Intent(in) :: H201(NAO,NAO,NAO), H102(NAO,NAO,NAO)
    Complex (Kind=pr), Intent(in) :: H300(NAO,NAO,NAO), H003(NAO,NAO,NAO)

    Integer     :: IAlloc
! Set up local copies of necessary variables used in derivative evaluation
   ! NAOBrd   = NAO
! y contains T1, T2
   ! nsBrd = NAO 
   ! ndBrd = NAO*(NAO-1)/2 
   ! nBrd  = nsBrd + ndBrd 
! Allocate and continue copying variables
    
    Allocate(UBrd(NAO), VBrd(NAO), MixVec(nBrd),H010Brd(NAO), H001Brd(NAO), H100Brd(NAO), &
               H020Brd(NAO,NAO),        &
               H101Brd(NAO,NAO), H200Brd(NAO,NAO),        &
               H002Brd(NAO,NAO),H011Brd(NAO,NAO), H110Brd(NAO,NAO),       &
               H030Brd(NAO,NAO,NAO),H111Brd(NAO,NAO,NAO),H120Brd(NAO,NAO,NAO), &
               H210Brd(NAO,NAO,NAO), H012Brd(NAO,NAO,NAO), &
               H021Brd(NAO,NAO,NAO),H201Brd(NAO,NAO,NAO), H102Brd(NAO,NAO,NAO), &
               H300Brd(NAO,NAO,NAO), H003Brd(NAO,NAO,NAO), &
               Stat=IAlloc)

    If(IAlloc /= 0) Stop "Could not allocate in SetUpBroyden"
    UBrd = BCSU
    VBrd = BCSV
    H010Brd = H010
    H001Brd = H001
    H100Brd = H100
    H020Brd = H020
    H101Brd = H101
    H200Brd = H200
    H002Brd = H002
    H011Brd = H011
    H110Brd = H110
    H030Brd = H030
    H111Brd = H111
    H210Brd = H210
    H120Brd = H120
    H012Brd = H012
    H021Brd = H021
    H201Brd = H201
    H102Brd = H102
    H300Brd = H300
    H003Brd = H003
    H000Brd = H000
    Return
    End Subroutine SetUpBroyden 

    Subroutine ShutDownBroyden
    Implicit None
    Integer       :: IAlloc
    Deallocate(H010Brd, H001brd, H100Brd, &
               H020Brd,        &
               H101Brd, H200Brd,        &
               H002Brd,H011Brd, H110Brd,       &
               H030Brd,H111Brd,H120Brd, &
               H210Brd, H012Brd, &
               H021Brd,H201Brd, H102Brd, &
               H300Brd, H003Brd, &
               UBrd, VBrd, MixVec,                    &
               Stat=IAlloc)

   
    If(IAlloc /= 0) Stop "Could not deallocate in ShutDownBroyden"
    Return
    End Subroutine ShutDownBroyden

    Subroutine Mat2Vec(y,T1,T2,T3,NAOBrd,nBrd)
    Implicit None
    Integer , Intent(In) :: NAOBrd, nBrd
    Complex (Kind=pr),   Intent(In)  :: T1(NAOBrd)
    Complex (Kind=pr),   Intent(In)  :: T2(NAOBrd,NAOBrd)
    Complex (Kind=pr),   Intent(In)  :: T3(NAOBrd,NAOBrd,NAOBrd)
    Complex (Kind=pr),   Intent(Out) :: y(nBrd)
    Integer       :: IamNum
    Integer       :: I, J,K
    IamNum = 0
! Put T1 into y
    Do I = 1, NAOBrd
      IamNum    = IamNum + 1
      y(IamNum) = T1(I)
    EndDo
! Put T2tilde into y
    Do J = 2, NAOBrd
    Do I = 1, J-1 
      IamNum    = IamNum + 1
      y(IamNum) = T2(I,J)
    EndDo
    EndDo

    Do K = 3, NAOBrd
    Do J = 2, K-1
    Do I = 1, J-1
        IamNum    = IamNum + 1
        y(IamNum) = T3(I,J,K)
    EndDo
    EndDo
    EndDo
! Check compatibility between dimension of y and Utildes
  !  Write(*,*) "In Mat2Vec NAOBrd = ", NAOBrd, " nBrd = ", nBrd, "IamNum = ", IamNum

    If(nBrd.ne.IamNum) Stop "Mismatch:Dimension of y is not compatible within Broyden"
    Return
    End Subroutine Mat2Vec

    Subroutine Vec2Mat(y,T1,T2,T3,NAOBrd,nBrd)
    Implicit None
    Integer , Intent(In) :: NAOBrd, nBrd
    Complex (Kind=pr),   Intent(In)  :: y(nBrd)
    Complex (Kind=pr),   Intent(Out) :: T1(NAOBrd)
    Complex (Kind=pr),   Intent(Out) :: T2(NAOBrd,NAOBrd)
    Complex (Kind=pr),   Intent(Out) :: T3(NAOBrd,NAOBrd,NAOBrd)
    Complex (Kind=pr)    :: tmp 
    Integer              :: IamNum
    Integer              :: I, J, K
    IamNum = 0
! Get T1 from y
    Do I = 1, NAOBrd
      IamNum = IamNum + 1
      T1(I)  = y(IamNum) 
    EndDo
! Get T2 from y
    T2 = Zero
    Do J = 2, NAOBrd
    Do I = 1, J-1 
      IamNum = IamNum + 1
      tmp    = y(IamNum)
      T2(I,J)= tmp
      T2(J,I)= tmp
    EndDo
    EndDo

    T3 = zero
    Do K = 3, NAOBrd
    Do J = 2, K-1
    Do I = 1, J-1
        IamNum = IamNum + 1
        T3(I,J,K)    = y(IamNum)
        
    EndDo
    EndDo
    EndDo
! Check compatibility between dimension of y and Utildes
   
    If(nBrd.ne.IamNum) Stop "Dimension of y is not compatible within Broyden "
    Return
    End Subroutine Vec2Mat

    Subroutine EvalF(x,fx,NAOBrd,nBrd)
    Implicit None
    Integer, Intent (In) :: nBrd, NAOBrd
    Complex (Kind=pr),   Intent(In)  :: x(nBrd)
    Complex (Kind=pr),   Intent(Out) :: fx(nBrd)
    Complex (Kind=pr),   Allocatable :: T1(:), T2(:,:), T3(:,:,:)
    Complex (Kind=pr),   Allocatable :: Res1(:), Res2(:,:), Res3(:,:,:)
    Integer                          :: IAlloc
! Allocate
    Allocate(T1(NAOBrd), T2(NAOBrd,NAOBrd), T3(NAOBrd,NAOBrd,NAOBrd),     &
             Res1(NAOBrd), Res2(NAOBrd,NAOBrd), Res3(NAOBrd,NAOBrd,NAOBrd), &
             Stat=IAlloc)
    If(IAlloc /= 0) Stop "Could not allocate in EvalF"
! Determine what T1 and T2 are
   ! Write(*,*) "Size of T1 = ", Size(T1)
   ! Write(*,*) "Size of T2 = ", Size(T2,1), Size(T2,2)
   ! Write(*,*) "NAOBrd = ", NAOBrd
   ! Write(*,*) "nBrd = ", nBrd

    Call Vec2Mat(x,T1,T2,T3,NAOBrd,nBrd)
! Build Residuals 
    If(DoCCD) T1 = Zero ! CCD!
    If(DoCCSD) T3 = zero
    Call CCSD_SFS(EneBrd,Res1,Res2,Res3,T1,T2,T3,NAOBrd,  &
        H001Brd,H100Brd,H010Brd,H101Brd,H020Brd,H200Brd,H002Brd,H110Brd,H011Brd, &
        H030Brd,H111Brd,H120Brd,H210Brd,H021Brd,H012Brd,H201Brd,H102Brd,H003Brd,H300Brd)
    EneBrd = EneBrd + H000Brd

    If(DoCCD) Res1 = Zero ! CCD!
    If(DoCCSD) Res3 = Zero ! CCSD
! Put dUtilde into dy
    Call Mat2Vec(fx,Res1,Res2,Res3,NAOBrd,nBrd)
! Deallocate
    Deallocate(T1, Res1, T2, Res2,T3,Res3,  & 
               Stat=IAlloc)
    If(IAlloc /= 0) Stop "Could not deallocate in EvalF"
    Return
    End Subroutine EvalF

    Subroutine BroydenStep(xnew, xold, fold, mix, w0, w, dF, dx, NIter, nBrd, pBrd, damp)
    Implicit None

    Integer,           Intent(In)  :: NIter, nBrd, pBrd
    Real(Kind=pr),     Intent(In)  :: mix, w0, damp
    Complex (Kind=pr), Intent(In)  :: xold(nBrd), fold(nBrd)
    Complex (Kind=pr), Intent(In)  :: dF(nBrd,pBrd), dx(nBrd,pBrd)
    Real (Kind=pr),    Intent(In)  :: w(pBrd)
    Complex (Kind=pr), Intent(Out) :: xnew(nBrd)

    Complex (Kind=pr), Allocatable :: AMat(:,:), BetaMat(:,:), GammaVec(:)
    Complex (Kind=pr), Allocatable :: UMat(:,:), CVec(:)
    Complex (kind=pr), Allocatable :: xtrial(:),step(:)
    Integer         :: I, J
    Integer         :: IAlloc
    Real(kind=pr) :: step_norm, step_max
    Allocate(xtrial(nBrd), step(nBrd), Stat=IAlloc)
    If (IAlloc/=0) Stop "Could not allocate temporaries in BroydenStep"
      
    
    !xnew = xold
    xtrial = xold
    
    If (pBrd > 0) Then

        do J = 1, nBrd
          xtrial(J) = xtrial(J) + mix * MixVec(J) * fold(J)
        end do
        ! Allocate arrays
        Allocate(AMat(pBrd,pBrd), UMat(nBrd,pBrd), CVec(pBrd), BetaMat(pBrd,pBrd), GammaVec(pBrd), Stat=IAlloc)
        If (IAlloc /= 0) Stop "Could not allocate in BroydenStep"

        ! Build matrix AMat: a_kn = w_k * w_n * <dF_n | dF_k>
        Do I = 1, pBrd
        Do J = 1, pBrd
           AMat(I,J) = w(I)*w(J)*Dot_Product(dF(:,J), dF(:,I))
        End Do
        End Do

        ! Add w0^2 * Identity to AMat diagonal
        Do I = 1, pBrd
            AMat(I,I) = AMat(I,I) + w0*w0
        End Do

        ! Invert AMat into BetaMat
        Call InvertC(AMat, BetaMat, pBrd)

        ! Compute CVec: c_k = w_k * <dF_k | fold>
        Do I = 1, pBrd
            CVec(I) = w(I)*Dot_Product(dF(:,I), fold)
        End Do

        ! Compute GammaVec: gamma_n = sum_k c_k * beta_k_n
        GammaVec = (0.0_pr, 0.0_pr)
        Do J = 1, pBrd
        Do I = 1, pBrd
            GammaVec(J) = GammaVec(J) + CVec(I)*BetaMat(I,J)
        End Do
        End Do

        ! Compute UMat: u_n = mix * MixVec * dF_n + dx_n
        Do I = 1, pBrd
        Do J = 1, nBrd
           UMat(J,I) = mix * MixVec(J) * dF(J,I) + dx(J,I)
        End Do
        End Do

        ! Apply update:
        ! First add mixing of residual fold (scaled by mix)
        !Do J = 1, nBrd
        !    xnew(J) = xnew(J) + mix * MixVec(J) * fold(J)
        !End Do

        ! Apply Broyden corrections
        Do I = 1, pBrd
            xtrial = xtrial -  w(I) * GammaVec(I) * UMat(:,I)
        End Do

        ! Deallocate arrays
        Deallocate(AMat, UMat, CVec, BetaMat, GammaVec, Stat=IAlloc)
        If (IAlloc /= 0) Stop "Could not deallocate in BroydenStep"

    Else
        ! No previous Broyden history, do simple mixing
        Do J = 1, nBrd
            xtrial(J) = xtrial(J) + mix * MixVec(J) * fold(J)
        End Do
    End If

    ! ===== Global damping (applies to both branches) =====
    
    xnew = xold + damp * (xtrial -xold)
   
    Deallocate(xtrial, step, Stat=IAlloc)
    If (IAlloc /= 0) Stop "Could not deallocate temporaries in BroydenStep"


    Return
End Subroutine BroydenStep



    Subroutine UpdateBroyden(dF,dx,fnew,fold,xnew,xold,nBrd, pBrd)
    Implicit None
    Integer, Intent(In) :: nBrd, pBrd
    Complex(Kind=pr), Intent(In)    :: fnew(nBrd), fold(nBrd)
    Complex(Kind=pr), Intent(In)    :: xnew(nBrd), xold(nBrd)
    Complex(Kind=pr), Intent(InOut) :: dF(nBrd,pBrd), dx(nBrd,pBrd) 
    Real(Kind=pr)                   :: norm
    Integer                         :: I
    If (pBrd>0) then
! Put all dF and dx vec one index to the left
    Do I = 1, pBrd-1
      dF(:,I) = dF(:,I+1)
      dx(:,I) = dx(:,I+1)
    EndDo
! Build norm = |fnew-fold| 
    norm = sqrt(Dot_Product(fnew-fold,fnew-fold))
! dx = (xnew - xold) / norm 
    dx(:,pBrd) = (xnew-xold)/norm
! df = (fnew - fold) / norm 
    dF(:,pBrd) = (fnew-fold)/norm
    EndIf
    Return
    End Subroutine UpdateBroyden

    subroutine floor_complex(D, Delta)
    complex(kind=pr), intent(inout) :: D
    real(kind=pr),    intent(in)    :: Delta
    real(kind=pr) :: mag
    mag = abs(D)

    ! If magnitude below threshold, raise magnitude but preserve phase
    if (mag < Delta) then
        if (mag < tiny(1.0_pr)) then
            ! Avoid direction ambiguity when D == 0
            D = cmplx(Delta, 0.0_pr, kind=pr)
        else
            ! Scale up while preserving complex direction
            D = D * (Delta / mag)
        end if
    end if
    end subroutine floor_complex

    !    ! ---------- Helpers to print amplitudes & residuals ----------
Subroutine PrintAmpsResFromVec(unit,x,fx,NAOBrd,nBrd,nsBrd,ndBrd,ntBrd)
  Use, Intrinsic :: iso_fortran_env, Only: output_unit
  Implicit None
  Integer,           Intent(In) :: unit, NAOBrd, nBrd, nsBrd, ndBrd, ntBrd
  Complex(Kind=pr),  Intent(In) :: x(nBrd), fx(nBrd)
  Complex(Kind=pr), Allocatable :: T1(:), T2(:,:),T3(:,:,:)
  Complex(Kind=pr), Allocatable :: R1(:), R2(:,:), R3(:,:,:)
  Integer                       :: IAlloc

  Allocate(T1(NAOBrd), T2(NAOBrd,NAOBrd), T3(NAOBrd,NAOBrd,NAOBrd),&
           R1(NAOBrd), R2(NAOBrd,NAOBrd), R3(NAOBrd,NAOBrd,NAOBrd),&
           Stat=IAlloc)
  If(IAlloc /= 0) Stop "Could not allocate in PrintAmpsResFromVec"

  ! Unpack current amplitudes and residuals using your Vec2Mat
  Call Vec2Mat(x ,T1,T2,T3,NAOBrd,nBrd)
  Call Vec2Mat(fx,R1,R2,R3,NAOBrd,nBrd)

  Call PrintAmpsResCore(unit,'T', T1,T2,T3, NAOBrd)
  Call PrintAmpsResCore(unit,'R', R1,R2, R3,NAOBrd)

  Deallocate(T1,T2,T3,R1,R2,R3, Stat=IAlloc)
  If(IAlloc /= 0) Stop "Could not deallocate in PrintAmpsResFromVec"
End Subroutine PrintAmpsResFromVec


    Subroutine PrintAmpsResCore(unit, tag, A1,A2,A3, NAO)
  Implicit None
  Integer,           Intent(In) :: unit, NAO
  Character(len=*),  Intent(In) :: tag   ! 'T' or 'R'
  Complex(Kind=pr),  Intent(In) :: A1(NAO)
  Complex(Kind=pr),  Intent(In) :: A2(NAO,NAO)
  Complex(Kind=pr),  Intent(In) :: A3(NAO,NAO,NAO)

  Real(Kind=pr) :: n1,n2,n3,n4, m1,m2,m3,m4
  Integer :: i,j,k,l, nshow

  n1 = sqrt( sum( abs(A1)**2 ) )
  n2 = sqrt( sum( abs(A2)**2 ) )
  n3 = sqrt( sum( abs(A3)**2 ) )

  m1 = maxval( abs(A1) )
  m2 = maxval( abs(A2) )
  m3 = maxval( abs(A3) )

  Write(unit,'(A)')        '------------- '//trim(tag)//' summary -------------'
  Write(unit,'(A,1PE12.4)') trim(tag)//'1: ||.||2 = ', n1
  Write(unit,'(A,1PE12.4)') trim(tag)//'2: ||.||2 = ', n2
  Write(unit,'(A,1PE12.4)') trim(tag)//'3: ||.||2 = ', n3

  Write(unit,'(A,1PE12.4)') 'max|'//trim(tag)//'1| = ', m1
  Write(unit,'(A,1PE12.4)') 'max|'//trim(tag)//'2| = ', m2
  Write(unit,'(A,1PE12.4)') 'max|'//trim(tag)//'3| = ', m3


  ! Print a few sample entries (packed ordering)
  nshow = min(5, NAO)
  Write(unit,'(A)') 'Samples '//trim(tag)//'1(i):'
  do i=1,nshow
    Write(unit,'(A,I4,A,2(1PE12.4,1X))') '  i=',i,'  ', Real(A1(i)), Aimag(A1(i))
  end do

  Write(unit,'(A)') 'Samples '//trim(tag)//'2(i<j):'
  do j=2,min(NAO,1+nshow)
    do i=1,min(j-1,nshow)
      Write(unit,'(A,2I4,A,2(1PE12.4,1X))') '  (i,j)=',i,j,'  ', Real(A2(i,j)), Aimag(A2(i,j))
    end do
  end do
End Subroutine PrintAmpsResCore



!    SUBROUTINE LINESCAN(XOLD,XNEW)
!    IMPLICIT NONE
!    COMPLEX(KIND=PR), INTENT(IN)    :: XNEW(NBRD), XOLD(NBRD)
!    COMPLEX(KIND=PR), ALLOCATABLE   :: XINTER(:), FINTER(:)
!    REAL(KIND=PR),    ALLOCATABLE   :: ABSF(:)
!    REAL (KIND=PR)      :: SCAL
!    INTEGER             :: I, J, NINTER
!    NINTER = 10
!    ALLOCATE(XINTER(NBRD), FINTER(NBRD), ABSF(NINTER))
!    ABSF = ZERO
!    DO I = 1, NINTER
!      SCAL = REAL(I-1)/REAL(NINTER-1)
!      XINTER = (ONE-SCAL)*XOLD + SCAL*XNEW
!      CALL EVALF(XINTER,FINTER)
!      DO J = 1, NBRD
!        ABSF(I) = ABSF(I) + ABS(FINTER(J))**2
!      ENDDO
!      ABSF(I) = SQRT(ABSF(I))
!    ENDDO
!!   WRITE(*,*) "|F|", ABSF
!    CALL OUTMAT(6,1,NINTER,3,ABSF,"|F|") 
!    RETURN
!    END SUBROUTINE LINESCAN
!

    End Module Broyden_SFS_CCSDT
