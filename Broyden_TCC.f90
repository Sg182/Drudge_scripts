    Module Broyden_TCC
! Implementation of modified Broyden's methods
! Ref: PRC 78, 014318; PRB, 38, 12807 
    Use Precision
    Use Constants
    Use CCRes
    Implicit None
    Private
    Public  :: BroydenIterTCC,Mat2Vec, Vec2Mat, BroydenStep, UpdateBroyden,ShutDownBroyden, SetUpBroyden, EvalF
! nBrd: # of unknowns, pBrd: # of Broyden step stored
    Integer                        :: nBrd, pBrd
! Local copies of PUCC variables   
    Integer                        :: nsBrd, ndBrd, ntBrd, nqBrd
    Integer                        :: NAOBrd
    Complex(Kind=pr)               :: H00Brd
    Complex(Kind=pr), Allocatable  :: H20Brd(:), H11Brd(:), H02Brd(:)
    Complex(Kind=pr), Allocatable  :: H40Brd(:,:), H04Brd(:,:)
    Complex(Kind=pr), Allocatable  :: H31Brd(:,:), H13Brd(:,:)
    Complex(Kind=pr), Allocatable  :: H22Brd(:,:), HT22Brd(:,:)
    Complex(Kind=pr), Allocatable  :: UBrd(:), VBrd(:)
    Complex(Kind=pr), Allocatable  :: MixVec(:)
    Complex(Kind=pr)               :: EneBrd
    Logical                        :: DoCCD = .true.

    Contains

    Subroutine BroydenIterTCC(Ene,T1,T2,BCSU,BCSV,NAO,pBrdIn,DoCCDIn, &
                              H00,H20,H11,H02,H40,H31,H22,HT22,H13,H04)!,naobrd,nbrd) ! I made changes here included naobrd,nbrd
    Implicit None
    Integer,           Intent(In)    :: NAO, pBrdIn! ,naobrd,nbrd ! added naobrd, nbrd
    Complex (Kind=pr), Intent(Out)   :: Ene
    Real(Kind=pr) :: damp

    Complex (Kind=pr), Intent(InOut) :: T1(NAO)
    Complex (Kind=pr), Intent(InOut) :: T2(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: BCSU(NAO), BCSV(NAO)
    Complex (Kind=pr), Intent(In)    :: H00, H20(NAO), H11(NAO), H02(NAO)
    Complex (Kind=pr), Intent(In)    :: H40(NAO,NAO), H31(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: H04(NAO,NAO), H13(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: H22(NAO,NAO), HT22(NAO,NAO)
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

    IF (DoCCD) Write(*,*) "Enter Broyden CCD"
 ! y contains T1 T2
    NAOBrd = NAO
    nsBrd = NAO                 !Made changes here
    ndBrd = NAO*(NAO-1)/2       !
    nBrd  = nsBrd + ndBrd       !
    
    !print*, "H11 is ", real(H11)
    !print*, "H22b is ", real(HT22)
    !print*, "H22 is ", real(H22)
    !print*, "H02 is ",real(H02)
    !print*, "H20 is ",real(H20)
    !print*, "H31 is ",real(H31)
    !print*, "H13 is ",real(H13)
    !print*, "H04 is ",real(H04)
    !print*, "H40 is ",real(H40)
    
   ! Write(*,*) "After SetUp: NAOBrd = ", NAOBrd, " nBrd = ", nBrd

    Call SetUpBroyden(BCSU,BCSV,NAO, &
                      H00,H20,H11,H02,H40,H31,H22,HT22,H13,H04)
    pBrd = pBrdIn
    
    print*, "T1 at the beginning is: ", maxval(real(T1))
    print*, "Is it really?"
    print*, "T2 at the beginning is: ", maxval(real(T2))
   
 
! Allocate
    Allocate(xold(nBrd), xnew(nBrd), fold(nBrd), fnew(nBrd),    &
             Stat=IAlloc)
    If(IAlloc/=0) Stop "Could not allocate in BroydenIter"
    If(pBrd>0) then
      Allocate(dF(nBrd,pBrd), dx(nBrd,pBrd), w(pBrd),             &
               Stat=IAlloc)
      If(IAlloc/=0) Stop "Could not allocate in BroydenIter"
      dF = Zero !added these inside if
      dx = Zero
      w = One
    EndIf
!   Write(*,*) "Entered BCS CCSD"
    Open(8,File='Output',Position='Append')
    Write(8,1020)
    Write(8,1010)
    Write(8,1020)
    Write(8,1030)
! Initialize 
    If(DoCCD) T1 = Zero ! CCD!
  
    Call Mat2Vec(xold,T1,T2,NAOBrd,nBrd)
    NIter = 0
    dT = 1.0_pr
    !dF = Zero  !remove comment after debugging
    !dx = Zero 
    !w  = One
    !w0 = 0.01_pr
    !mix= 0.50_pr
    w0 = 0.10_pr
    mix=0.50_pr
    Call EvalF(xold,fold,NAOBrd,nBrd)
    !ResOld = Sqrt(Dot_Product(fold,fold))
    ResOld = sqrt(real(dot_product(fold, fold), kind=pr))
    !ResNew = sqrt(real(dot_product(conjg(fnew), fnew), kind=pr))
    ResNew = ResOld
    Write(8,1040) Real(EneBrd),NIter,dT
! Build the mixing vector (Approximate -J^-1)
    Do I = 1, NAO
      Denom = HT22(I,I) + Two*H11(I) + Four*H22(I,I) 
      call floor_complex(Denom, 1.0_pr)   !was 0.30_pr for both
      T1(I) = -One/Denom 
    Do J = 1, NAO
      Denom = (HT22(I,I)+HT22(J,J)) + Two*(H11(I)+H11(J)) &
            + Four*(H22(I,I)+H22(J,J)+Two*H22(I,J))
      call floor_complex(Denom, 1.0_pr)   
      T2(I,J) = -One/Denom 
    EndDo
    EndDo
   

    Call Mat2Vec(MixVec,T1,T2,NAOBrd,nBrd)
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
      !write(*,*) 'max|fnew| = ', maxval(abs(fnew))
      !write(*,*) '||fnew||2 = ', sqrt(sum(abs(fnew)**2)) !CHANGes here
      if (.not.(real(EneBrd) == real(EneBrd)) .or. &
          .not.(aimag(EneBrd) == aimag(EneBrd))) then
         stop 'Broyden diverged: EneBrd is NaN'
      end if

      ! Print every 10 iterations to avoid huge logs; change 10 -> 1 to print every iter
      If (mod(NIter,10)==1 .or. NIter<=3) then
      Call PrintAmpsResFromVec(8, xnew, fnew, NAOBrd, nBrd, nsBrd, ndBrd, ntBrd)
      End If

      ResOld = ResNew
      ResNew = Sqrt(Dot_Product(fnew,fnew))
! Check convergence
      dTold = dT
      dT = MaxVal(Abs(fnew))
      if (.not.(dT == dT)) then
         stop 'Broyden diverged: dT is NaN'
      end if

      if (dT > 1.0e3_pr) then
         stop 'Broyden diverged: residual exploded'
      end if
! Update dF and dx matrices
     ! Write(*,*) "nBrd = ", nBrd
      Call UpdateBroyden(dF,dx,fnew,fold,xnew,xold,nBrd, pBrd)
      Write(8,1040) Real(EneBrd),NIter,dT
      
      If (NIter > CycMax) then
        Call Vec2Mat(xnew,T1,T2,NAOBrd,nBrd)
! checkpoint T1 and T2
        
        Open(70,File="Chk_T1",status="Replace")
        Write(70,*) T1
        Close(70)
        Open(70,File="Chk_T2",status="Replace")
        Write(70,*) T2
        Close(70)
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

      Call Vec2Mat(xnew,T1,T2,NAOBrd,nBrd)
      Open(70,File="Chk_T1",status="Replace")
      Write(70,*) T1
      Close(70)
      Open(70,File="Chk_T2",status="Replace")
      Write(70,*) T2
      Close(70)
! End of Debug
    EndDo
  

    Call Vec2Mat(xnew,T1,T2,NAOBrd,nBrd)
    Ene = EneBrd
    Write(8,1020)
    Write(8,1050) NIter
    Write(8,1070) Real(EneBrd)
    Write(8,1080) Aimag(EneBrd)
    Write(8,1000)
    !Close(8)

    !================= === FINAL RESIDUAL CHECK (authoritative) ==================================================================
 BLOCK
Complex(Kind=pr), Allocatable :: fx_final(:)
Complex(Kind=pr), Allocatable :: R1fin(:), R2fin(:,:)
Real(Kind=pr) :: maxAll, maxR1, maxR2


Allocate(fx_final(nBrd), R1fin(NAOBrd), R2fin(NAOBrd,NAOBrd), Stat=IAlloc)
If (IAlloc/=0) Stop "Could not allocate in final residual check"

! Evaluate F at the *final* amplitudes (xnew is already the final)
!Call Mat2Vec(xold,T1,T2,NAOBrd,nBrd)    ! pack current T back to xold
Call EvalF(xold, fx_final, NAOBrd, nBrd)   ! compute fresh residuals

Call Vec2Mat(fx_final, R1fin, R2fin, NAOBrd, nBrd)

maxR1 = maxval(abs(R1fin))
! doubles packing i<j only:
maxR2 = 0.0_pr
Do j=2,NAOBrd
  Do i=1,j-1
    maxR2 = max(maxR2, abs(R2fin(i,j)))
  End Do
End Do

maxAll = max( maxR1, maxR2 )

write(8,'(A,1PE12.4)') 'FINAL max|Res1| = ', maxR1
write(8,'(A,1PE12.4)') 'FINAL max|Res2| = ', maxR2
write(8,'(A,1PE12.4)') 'FINAL max|Res | = ', maxAll
call PrintAmpsResFromVec(8,xnew,fx_final,NAOBrd,nBrd, nsBrd,ndBrd,ntBrd)

! If not actually converged, flag it clearly:
If (maxAll > TolMax) Then
  write(8,'(A)') '*** WARNING: Residual > TolMax at exit; NOT converged by residual! ***'
End If

Deallocate(fx_final, R1fin, R2fin, Stat=IAlloc)
If (IAlloc/=0) Stop "Could not deallocate in final residual check"
END BLOCK
Close(8)
! ==========================================================================================================================
! =========================================================================================================================
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
                            H00,H20,H11,H02,H40,H31,H22,HT22,H13,H04)
    Implicit None
    Integer,           Intent(In)    :: NAO
    Complex (Kind=pr), Intent(In)    :: BCSU(NAO), BCSV(NAO)
    Complex (Kind=pr), Intent(In)    :: H00, H20(NAO), H11(NAO), H02(NAO)
    Complex (Kind=pr), Intent(In)    :: H40(NAO,NAO), H31(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: H04(NAO,NAO), H13(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: H22(NAO,NAO), HT22(NAO,NAO)
    Integer     :: IAlloc
! Set up local copies of necessary variables used in derivative evaluation
   ! NAOBrd   = NAO
! y contains T1, T2
   ! nsBrd = NAO 
   ! ndBrd = NAO*(NAO-1)/2 
   ! nBrd  = nsBrd + ndBrd 
! Allocate and continue copying variables
    Allocate(UBrd(NAO), VBrd(NAO), MixVec(nBrd),    &
             H20Brd(NAO), H11Brd(NAO), H02Brd(NAO), &
             H40Brd(NAO,NAO), H31Brd(NAO,NAO),      &
             H13Brd(NAO,NAO), H04Brd(NAO,NAO),      &
             H22Brd(NAO,NAO), HT22Brd(NAO,NAO),     & 
             Stat=IAlloc)
    If(IAlloc /= 0) Stop "Could not allocate in SetUpBroyden"
    UBrd = BCSU
    VBrd = BCSV
    H00Brd = H00 
    H11Brd = H11
    H20Brd = H20
    H02Brd = H02
    H22Brd = H22
    HT22Brd = HT22 
    H40Brd = H40
    H04Brd = H04
    H31Brd = H31
    H13Brd = H13
    Return
    End Subroutine SetUpBroyden 

    Subroutine ShutDownBroyden
    Implicit None
    Integer       :: IAlloc
    Deallocate(H20Brd, H11Brd, H02Brd,  &
               H40Brd, H04Brd, H31Brd, H13Brd,  &
               H22Brd, HT22Brd,         &
               UBrd, VBrd, MixVec,      &
               Stat=IAlloc)
    If(IAlloc /= 0) Stop "Could not deallocate in ShutDownBroyden"
    Return
    End Subroutine ShutDownBroyden

    Subroutine Mat2Vec(y,T1,T2,NAOBrd,nBrd)
    Implicit None
    Integer , Intent(In) :: NAOBrd, nBrd
    Complex (Kind=pr),   Intent(In)  :: T1(NAOBrd)
    Complex (Kind=pr),   Intent(In)  :: T2(NAOBrd,NAOBrd)
    Complex (Kind=pr),   Intent(Out) :: y(nBrd)
    Integer       :: IamNum
    Integer       :: I, J
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
! Check compatibility between dimension of y and Utildes
  !  Write(*,*) "In Mat2Vec NAOBrd = ", NAOBrd, " nBrd = ", nBrd, "IamNum = ", IamNum

    If(nBrd.ne.IamNum) Stop "Mismatch:Dimension of y is not compatible within Broyden"
    Return
    End Subroutine Mat2Vec

    Subroutine Vec2Mat(y,T1,T2,NAOBrd,nBrd)
    Implicit None
    Integer , Intent(In) :: NAOBrd, nBrd
    Complex (Kind=pr),   Intent(In)  :: y(nBrd)
    Complex (Kind=pr),   Intent(Out) :: T1(NAOBrd)
    Complex (Kind=pr),   Intent(Out) :: T2(NAOBrd,NAOBrd)
    Complex (Kind=pr)    :: tmp 
    Integer              :: IamNum
    Integer              :: I, J
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
! Check compatibility between dimension of y and Utildes
   
    If(nBrd.ne.IamNum) Stop "Dimension of y is not compatible within Broyden "
    Return
    End Subroutine Vec2Mat

    Subroutine EvalF(x,fx,NAOBrd,nBrd)
    Implicit None
    Integer, Intent (In) :: nBrd, NAOBrd
    Complex (Kind=pr),   Intent(In)  :: x(nBrd)
    Complex (Kind=pr),   Intent(Out) :: fx(nBrd)
    Complex (Kind=pr),   Allocatable :: T1(:), T2(:,:)
    Complex (Kind=pr),   Allocatable :: Res1(:), Res2(:,:)
    Integer                          :: IAlloc
! Allocate
    Allocate(T1(NAOBrd), T2(NAOBrd,NAOBrd),     &
             Res1(NAOBrd), Res2(NAOBrd,NAOBrd), &
             Stat=IAlloc)
    If(IAlloc /= 0) Stop "Could not allocate in EvalF"
! Determine what T1 and T2 are
   ! Write(*,*) "Size of T1 = ", Size(T1)
   ! Write(*,*) "Size of T2 = ", Size(T2,1), Size(T2,2)
   ! Write(*,*) "NAOBrd = ", NAOBrd
   ! Write(*,*) "nBrd = ", nBrd

    Call Vec2Mat(x,T1,T2,NAOBrd,nBrd)
! Build Residuals 
    If(DoCCD) T1 = Zero ! CCD!
    Call BuildRes0(EneBrd,T1,T2,NAOBrd,  &             !This function Calculates Energy
          H20Brd,H11Brd,H02Brd,H40Brd,H31Brd,H22Brd,HT22Brd,H13Brd,H04Brd)
    EneBrd = EneBrd + H00Brd
    Call BuildRes1SD(Res1,T1,T2,NAOBrd,  &             !This function
          H20Brd,H11Brd,H02Brd,H40Brd,H31Brd,H22Brd,HT22Brd,H13Brd,H04Brd)
    Call BuildRes2SD(Res2,T1,T2,NAOBrd, &              !This function
          H20Brd,H11Brd,H02Brd,H40Brd,H31Brd,H22Brd,HT22Brd,H13Brd,H04Brd)
    If(DoCCD) Res1 = Zero ! CCD!
! Put dUtilde into dy
    Call Mat2Vec(fx,Res1,Res2,NAOBrd,nBrd)
! Deallocate
    Deallocate(T1, Res1, T2, Res2,  & 
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
    Complex (Kind=pr), Allocatable :: UMat(:,:), CVec(:), xtrial(:), step(:)
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
           !AMat(I,J) = w(I)*w(J)*Dot_Product(dF(:,J), dF(:,I))
           AMat(I,J) = w(I)*w(J)*sum(conjg(dF(:,J))*dF(:,I))
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
            !CVec(I) = w(I)*Dot_Product(dF(:,I), fold)
            CVec(I) = w(I)*sum(conjg(dF(:,I))*fold)
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

    step = xtrial - xold                 !added these lines
    step_max = maxval(abs(step))
    
    if (step_max > 0.02_pr) then
            step = step*(0.02_pr/step_max)
    endif                                  !upto here
    
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
    !norm = sqrt(Dot_Product(fnew-fold,fnew-fold)) !Changed HERE
    norm = sqrt(sum(abs(fnew-fold)**2))
    if (norm < 1.0e-12_pr) return
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
  if (mag < Delta) then
    if (mag == 0.0_pr) then
      D = cmplx(Delta, 0.0_pr, kind=pr)
    else
      D = D * (Delta / mag)   ! preserve phase
    end if
  end if
end subroutine floor_complex

subroutine scale_mix_blocks(mv, ns, nd, nt, nq, s1, s2, s3, s4)
    complex(kind=pr), intent(inout) :: mv(:)
    integer, intent(in)  :: ns, nd, nt, nq
    real(kind=pr), intent(in) :: s1, s2, s3, s4
    integer :: o1, o2, o3, o4
    o1 = 1; o2 = o1 + ns; o3 = o2 + nd; o4 = o3 + nt
    mv(o1:o2-1) = s1 * mv(o1:o2-1)
    mv(o2:o3-1) = s2 * mv(o2:o3-1)
    mv(o3:o4-1) = s3 * mv(o3:o4-1)
    mv(o4:)     = s4 * mv(o4:)
  end subroutine scale_mix_blocks

  subroutine cap_step(xold, xnew, ns, nd, nt, nq, cap1, cap2, cap3, cap4)
    complex(kind=pr), intent(in)    :: xold(:)
    complex(kind=pr), intent(inout) :: xnew(:)
    integer, intent(in) :: ns, nd, nt, nq
    real(kind=pr), intent(in) :: cap1, cap2, cap3, cap4
    integer :: o1,o2,o3,o4
    complex(kind=pr), allocatable :: dx(:)
    real(kind=pr) :: m
    o1=1; o2=o1+ns; o3=o2+nd; o4=o3+nt
    allocate(dx(size(xold))); dx = xnew - xold
    m = maxval(abs(dx(o1:o2-1))); if (m>cap1) dx(o1:o2-1) = dx(o1:o2-1) * (cap1/m)
    m = maxval(abs(dx(o2:o3-1))); if (m>cap2) dx(o2:o3-1) = dx(o2:o3-1) * (cap2/m)
    m = maxval(abs(dx(o3:o4-1))); if (m>cap3) dx(o3:o4-1) = dx(o3:o4-1) * (cap3/m)
    m = maxval(abs(dx(o4:   )));  if (m>cap4) dx(o4:   )  = dx(o4:   )  * (cap4/m)
    xnew = xold + dx
    deallocate(dx)
  end subroutine cap_step

!    ! ---------- Helpers to print amplitudes & residuals ----------
Subroutine PrintAmpsResFromVec(unit,x,fx,NAOBrd,nBrd,nsBrd,ndBrd,ntBrd)
  Use, Intrinsic :: iso_fortran_env, Only: output_unit
  Implicit None
  Integer,           Intent(In) :: unit, NAOBrd, nBrd, nsBrd, ndBrd, ntBrd
  Complex(Kind=pr),  Intent(In) :: x(nBrd), fx(nBrd)
  Complex(Kind=pr), Allocatable :: T1(:), T2(:,:)
  Complex(Kind=pr), Allocatable :: R1(:), R2(:,:)
  Integer                       :: IAlloc

  Allocate(T1(NAOBrd), T2(NAOBrd,NAOBrd),&
           R1(NAOBrd), R2(NAOBrd,NAOBrd),&
           Stat=IAlloc)
  If(IAlloc /= 0) Stop "Could not allocate in PrintAmpsResFromVec"

  ! Unpack current amplitudes and residuals using your Vec2Mat
  Call Vec2Mat(x ,T1,T2,NAOBrd,nBrd)
  Call Vec2Mat(fx,R1,R2,NAOBrd,nBrd)

  Call PrintAmpsResCore(unit,'T', T1,T2, NAOBrd)
  Call PrintAmpsResCore(unit,'R', R1,R2, NAOBrd)

  Deallocate(T1,T2,R1,R2, Stat=IAlloc)
  If(IAlloc /= 0) Stop "Could not deallocate in PrintAmpsResFromVec"
End Subroutine PrintAmpsResFromVec


Subroutine PrintAmpsResCore(unit, tag, A1,A2, NAO)
  Implicit None
  Integer,           Intent(In) :: unit, NAO
  Character(len=*),  Intent(In) :: tag   ! 'T' or 'R'
  Complex(Kind=pr),  Intent(In) :: A1(NAO)
  Complex(Kind=pr),  Intent(In) :: A2(NAO,NAO)
  
  Real(Kind=pr) :: n1,n2,n3,n4, m1,m2,m3,m4
  Integer :: i,j,k,l, nshow

  n1 = sqrt( sum( abs(A1)**2 ) )
  n2 = sqrt( sum( abs(A2)**2 ) )
 
  m1 = maxval( abs(A1) )
  m2 = maxval( abs(A2) )
  
  Write(unit,'(A)')        '------------- '//trim(tag)//' summary -------------'
  Write(unit,'(A,1PE12.4)') trim(tag)//'1: ||.||2 = ', n1
  Write(unit,'(A,1PE12.4)') trim(tag)//'2: ||.||2 = ', n2
 
  Write(unit,'(A,1PE12.4)') 'max|'//trim(tag)//'1| = ', m1
  Write(unit,'(A,1PE12.4)') 'max|'//trim(tag)//'2| = ', m2
  

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

    End Module Broyden_TCC

