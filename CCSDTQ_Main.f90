
      Subroutine NBCSCC(U,V,G,Lambda,E0,H11,H20,H02,H31,H13,H22,Hb22,H40,H04, &
                        NAO,NOcc,NPoints,pBrdIn,ECC)

        !f2py intent(in) :: U, V, G, Lambda, E0, H11, H20, H02, H31, H13, H22, Hb22, H40, H04
        !f2py intent(in) :: NAO, NOcc, NPoints, pBrdIn
        !f2py intent(out) :: ECC
          

      Use Precision
     ! Use BroydenTCC
      Use BroydenPCC
     ! Use Broyden_TCCSDT
      Use Broyden_CCSDTQ_Z
      Use Broyden_CCSDTQ
      Use pHFBCC
     ! Use FPUCC
      Use PNumber
      Implicit None
      Integer          , Intent(in)   :: NAO, NOcc
      Integer          , Intent(in)   :: NPoints, pBrdIn!,naobrd,nbrd
      Complex(Kind=pr),    Intent(in)   :: U(NAO), V(NAO)
      Real(kind=pr), Intent(in)       :: G,Lambda
      Complex (Kind=pr), Intent(in)   :: E0, H11(NAO), H20(NAO), H02(NAO)
      Complex (Kind=pr), Intent(in)   :: H40(NAO,NAO), H04(NAO,NAO)
      Complex (Kind=pr), Intent(in)   :: H31(NAO,NAO), H13(NAO,NAO)
      Complex (Kind=pr), Intent(in)   :: H22(NAO,NAO), Hb22(NAO,NAO)
      Complex (Kind=pr), Intent(out)  :: ECC
! Miscellaneous variables
      Real (Kind=pr) :: dQ, Energy, Lambdatmp
      Integer :: IAlloc, I, J, Iter, iG
! PUCC variables
      Integer :: TrunODE
      Complex (Kind=pr)              :: H00Cmplx, Ene
      Complex (Kind=pr), Allocatable :: H20Cmplx(:), H11Cmplx(:), H02Cmplx(:)
      Complex (Kind=pr), Allocatable :: H40Cmplx(:,:), H31Cmplx(:,:)
      Complex (Kind=pr), Allocatable :: H04Cmplx(:,:), H13Cmplx(:,:)
      Complex (Kind=pr), Allocatable :: H22Cmplx(:,:), HT22Cmplx(:,:)
      Complex (Kind=pr), Allocatable :: UCmplx(:), VCmplx(:)
      Complex (Kind=pr), Allocatable :: T1Cmplx(:), T2Cmplx(:,:),T3Cmplx(:,:,:), T4Cmplx(:,:,:,:)
      Complex (Kind=pr), Allocatable :: Z1Cmplx(:), Z2Cmplx(:,:),Z3Cmplx(:,:,:), Z4Cmplx(:,:,:,:)
      Complex (Kind=pr)              :: NP, NP2
      Logical :: DoCCDIn
! Restart variables
      Logical :: ExistsA, ExistsB, Exists
      
      DoCCDIn = .false.
      TrunODE = 2

!===============================!
! Prepare integrals and coeffs  !
!===============================!
      Allocate(H20Cmplx(NAO), H11Cmplx(NAO), H02Cmplx(NAO), &
               H40Cmplx(NAO,NAO), H31Cmplx(NAO,NAO),        &
               H13Cmplx(NAO,NAO), H04Cmplx(NAO,NAO),        &
               H22Cmplx(NAO,NAO), HT22Cmplx(NAO,NAO),       & 
               T1Cmplx(NAO), T2Cmplx(NAO,NAO),T3Cmplx(NAO,NAO,NAO),              &
               T4Cmplx(NAO,NAO,NAO,NAO),Z1Cmplx(NAO),Z2Cmplx(NAO,NAO), &
               Z3Cmplx(NAO,NAO,NAO),Z4Cmplx(NAO,NAO,NAO,NAO), UCmplx(NAO), VCmplx(NAO),                    &
               Stat=IAlloc)
      If(IAlloc /= 0) Stop "Could not allocate in main!"

      
! Cmplexify the variables
      UCmplx = U
      VCmplx = V
      H11Cmplx = H11
      H20Cmplx = H20
      H02Cmplx = H02
      H22Cmplx = H22
      HT22Cmplx = Hb22
      H40Cmplx = H40
      H04Cmplx = H04
      H31Cmplx = H31
      H13Cmplx = H13
      H00Cmplx = E0
      T1Cmplx = (0.0_pr,0.0_pr)
      T2Cmplx = (0.0_pr,0.0_pr) 
      T3Cmplx = (0.0_pr,0.0_pr)
      T4Cmplx = (0.0_pr,0.0_pr)
      Z1Cmplx = (0.0_pr,0.0_pr)
      Z2Cmplx = (0.0_pr,0.0_pr)
      Z3Cmplx = (0.0_pr,0.0_pr)
      Z4Cmplx = (0.0_pr,0.0_pr)

      Lambdatmp = Lambda
      Do I = 1, NAO
! Zero out the diagonal blocks of H31, H13, H40 and H04
      !  H31Cmplx(I,I) = Zero
      !  H13Cmplx(I,I) = Zero
        H04Cmplx(I,I) = Zero
        H40Cmplx(I,I) = Zero
      EndDo

!===============================!
!         BCS CCSD              !
!===============================!

      Inquire(File="Chk_T1", Exist=Exists)
        If (Exists) then
        Open(70,File="Chk_T1")
        Read(70,*) T1Cmplx
        Close(70)
        Else
        T1Cmplx = (0.0_pr,0.0_pr)
        EndIf

! ---- T2 ----
        Inquire(File="Chk_T2", Exist=Exists)
        If (Exists) then
        Open(70,File="Chk_T2")
        Read(70,*) T2Cmplx
        Close(70)
        Else
        T2Cmplx = (0.0_pr,0.0_pr)
        EndIf

! ---- T3 ----
        Inquire(File="Chk_T3", Exist=Exists)
        If (Exists) then
        Open(70,File="Chk_T3")
        Read(70,*) T3Cmplx
        Close(70)
        Else
        T3Cmplx = (0.0_pr,0.0_pr)
        EndIf

        Inquire(File="Chk_T4", Exist=Exists)
        If (Exists) then
        Open(70,File="Chk_T4")
        Read(70,*) T4Cmplx
        Close(70)
        Else
        T4Cmplx = (0.0_pr,0.0_pr)
        EndIf


        Inquire(File="Chk_Z1", Exist=Exists)
        If (Exists) then
        Open(70,File="Chk_Z1")
        Read(70,*) Z1Cmplx
        Close(70)
        Else
        Z1Cmplx = (0.0_pr,0.0_pr)
        EndIf

! ---- T2 ----
        Inquire(File="Chk_Z2", Exist=Exists)
        If (Exists) then
        Open(70,File="Chk_Z2")
        Read(70,*) Z2Cmplx
        Close(70)
        Else
        Z2Cmplx = (0.0_pr,0.0_pr)
        EndIf

! ---- T3 ----
        Inquire(File="Chk_Z3", Exist=Exists)
        If (Exists) then
        Open(70,File="Chk_Z3")
        Read(70,*) T3Cmplx
        Close(70)
        Else
        Z3Cmplx = (0.0_pr,0.0_pr)
        EndIf

        Inquire(File="Chk_Z4", Exist=Exists)
        If (Exists) then
        Open(70,File="Chk_Z4")
        Read(70,*) Z4Cmplx
        Close(70)
        Else
        Z4Cmplx = (0.0_pr,0.0_pr)
        EndIf





      print*,H00Cmplx
      !Call DIISIterTCC(Ene, T1Cmplx, T2Cmplx, UCmplx,VCmplx, NAO, DoCCDIn, &
      !                  H00Cmplx, H20Cmplx, H11Cmplx, H02Cmplx,H40Cmplx, H31Cmplx, H22Cmplx, HT22Cmplx, H13Cmplx, H04Cmplx)
     ! Call BCS_CCSD(UCmplx,VCmplx,G,T1Cmplx,T2Cmplx,NOcc,NAO,Ene,Lambdatmp)
    
    
      Call BroydenIterTCC(Ene,T1Cmplx,T2Cmplx,T3Cmplx,T4Cmplx,UCmplx,VCmplx,&
                    NAO,pBrdIn,DoCCDIn,                     &
                    H00Cmplx,H20Cmplx,H11Cmplx,H02Cmplx,    &
                    H40Cmplx,H31Cmplx,H22Cmplx,HT22Cmplx,   &
                    H13Cmplx,H04Cmplx)!,naobrd,nbrd)

      Call BroydenIterCCSDTQ_Z(Z1Cmplx,Z2Cmplx,Z3Cmplx,Z4Cmplx,T1Cmplx,T2Cmplx,T3Cmplx,T4Cmplx,UCmplx,VCmplx,&
                    NAO,pBrdIn,DoCCDIn,                     &
                    H00Cmplx,H20Cmplx,H11Cmplx,H02Cmplx,    &
                    H40Cmplx,H31Cmplx,H22Cmplx,HT22Cmplx,   &
                    H13Cmplx,H04Cmplx)!,naobrd,nbrd)

      Call CCNum(T1Cmplx,UCmplx,VCmplx,NP,NAO)
      !Print *, "E(CCSD) =", real(Ene,kind=pr)
      Print *, "N(CCSDTQ) =", real(NP,kind=pr)
 
      ECC = Ene
!===============================!
! Number projected BCS CCSD     !
!===============================!
!      Open(9,File="RKstat",Status="Replace")
!      Write(9,*) ''
!      Close(9)
!     T1Cmplx = Zero
!     T2Cmplx = Zero
!     Call TestWFD(UCmplx,VCmplx,T1Cmplx,T2Cmplx,NAO)
!     Call TestV1(UCmplx,VCmplx,NAO)
!     Call TestHV(UCmplx,VCmplx,NAO, &
!                 H00Cmplx,H20Cmplx,H11Cmplx,H02Cmplx,     &
!                 H40Cmplx,H31Cmplx,H22Cmplx,HT22Cmplx,    &
!                 H13Cmplx,H04Cmplx)
!     Call TestKern(T1Cmplx,T2Cmplx,UCmplx,VCmplx,NAO, &
!                   H00Cmplx,H20Cmplx,H11Cmplx,H02Cmplx,     &
!                   H40Cmplx,H31Cmplx,H22Cmplx,HT22Cmplx,    &
!                   H13Cmplx,H04Cmplx)
!      Call BuildH(UCmplx,VCmplx,G,Lambda,NAO,    &
!           H00Cmplx,H11Cmplx,H20Cmplx,H02Cmplx,  &
!           H31Cmplx,H13Cmplx,H22Cmplx,HT22Cmplx, &
!           H40Cmplx,H04Cmplx)
!      Call PbarHbarEne(T1Cmplx,T2Cmplx,UCmplx,VCmplx,   &
!                       NOcc,NAO,NPoints,TrunODE,        &
!                       H00Cmplx,H20Cmplx,H11Cmplx,H02Cmplx,     &
!                       H40Cmplx,H31Cmplx,H22Cmplx,HT22Cmplx,    &
!                       H13Cmplx,H04Cmplx)
!      Call Num(T1Cmplx,T2Cmplx,UCmplx,VCmplx,NP,NOcc,NAO,NPoints,TrunODE)
!      Call NumSqu(T1Cmplx,T2Cmplx,UCmplx,VCmplx,NP2,NOcc,NAO,NPoints,TrunODE)
!      Print *, "VarN(PCC) =", Real(NP2 - NP**2, kind=pr)

!===============================!
!           VAP PUCCSD          !
!===============================!
! Cmplexify the variables
!      UCmplx = Cmplx(U,Kind=pr)
!      VCmplx = Cmplx(V,Kind=pr)
!      H11Cmplx = Cmplx(H11,Kind=pr)
!      H20Cmplx = Cmplx(H20,Kind=pr)
!      H02Cmplx = Cmplx(H02,Kind=pr)
!      H22Cmplx = Cmplx(H22,Kind=pr)
!      HT22Cmplx = Cmplx(Hb22,Kind=pr)
!      H40Cmplx = Cmplx(H40,Kind=pr)
!      H04Cmplx = Cmplx(H04,Kind=pr)
!      H31Cmplx = Cmplx(H31,Kind=pr)
!      H13Cmplx = Cmplx(H13,Kind=pr)
!      H00Cmplx = Cmplx(E0,Kind=pr)
!      T1Cmplx = Zero
!      T2Cmplx = Zero 
!      Do I = 1, NAO
! Zero out the diagonal blocks of H31, H13, H40 and H04
!        H31Cmplx(I,I) = Zero
!        H13Cmplx(I,I) = Zero
!        H04Cmplx(I,I) = Zero
!        H40Cmplx(I,I) = Zero
!      EndDo
!      ExistsA = .True.
!      Inquire(File="Chk_PUCC_T1",Exist=Exists)
!      ExistsA = Exists .and. ExistsA
!      Inquire(File="Chk_PUCC_T2",Exist=Exists)
!      ExistsA = Exists .and. ExistsA
!      If (ExistsA) then
!        Open(70,File="Chk_PUCC_T1")
!        Read(70,*) T1Cmplx
!        Close(70)
!        Open(70,File="Chk_PUCC_T2")
!        Read(70,*) T2Cmplx
!        Close(70)
!      Else
!        T1Cmplx = Zero
!        T2Cmplx = Zero
!      EndIf
!      Call BroydenIterPCC(ECC,T1Cmplx,T2Cmplx,UCmplx,VCmplx,&
!                    NOcc,NAO,pBrdIn,DoCCDIn,NPoints,TrunODE,&
!                    H00Cmplx,H20Cmplx,H11Cmplx,H02Cmplx,    &
!                    H40Cmplx,H31Cmplx,H22Cmplx,HT22Cmplx,   &
!                    H13Cmplx,H04Cmplx)
!! Calculate the expection value
!      Call Num(T1Cmplx,T2Cmplx,UCmplx,VCmplx,NP,NOcc,NAO,NPoints,TrunODE)
!      Call NumSqu(T1Cmplx,T2Cmplx,UCmplx,VCmplx,NP2,NOcc,NAO,NPoints,TrunODE)
!      Print *, "VarN(PCC) =", Real(NP2 - NP**2, kind=pr)
      Deallocate(H20Cmplx, H11Cmplx, H02Cmplx,              &
                 H40Cmplx, H31Cmplx, H13Cmplx, H04Cmplx,    &
                 H22Cmplx, HT22Cmplx, T1Cmplx, T2Cmplx,     &
                 T3Cmplx,T4Cmplx,UCmplx, VCmplx,                            &
                 Z1Cmplx,Z2Cmplx,Z3Cmplx,Z4Cmplx,&
                 Stat=IAlloc)
      If(IAlloc /= 0) Stop "Could not deallocate in main!"
!===============================!
!  Deallocate and exit safely.  !
!===============================!

      End Subroutine 

