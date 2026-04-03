
Subroutine NBCSCC(U,V,G,Lambda,E0,H11,H20,H02,H31,H13,H22,Hb22,H40,H04, &
                  S00,S20p,S20q,S11p,S11q,S02p,S02q, &
                  S40,S31pq,S31qp,S22NN,ST22pq,ST22qp,S13pq,S13qp,S04, &
                  NAO,NOcc,NPoints,pBrdIn,nx,ny,iPBC,iCorr,ECC,Corr)

  !f2py intent(in) :: U, V, G, Lambda, E0, H11, H20, H02, H31, H13, H22, Hb22, H40, H04
  !f2py intent(in) :: NAO, NOcc, NPoints, pBrdIn, nx, ny, iPBC, iCorr
  !f2py intent(out) :: ECC, Corr

  Use Precision
  Use Broyden_TCC
  Use BroydenPCC
  Use Broyden_TCC_Z
  Use CC_Z_Test
  Use pHFBCC
  Use PNumber
  Use CCSDSpSq
  Use CCSDSpSqGrad
  Implicit None

  Integer          , Intent(in)   :: NAO, NOcc
  Integer          , Intent(in)   :: NPoints, pBrdIn
  Integer          , Intent(in)   :: nx, ny
  Integer          , Intent(in)   :: iPBC      ! 1 = PBC, 0 = OBC
  Integer          , Intent(in)   :: iCorr     ! 1 = compute Corr, 0 = skip
  Real   (Kind=pr), Intent(in)    :: U(NAO), V(NAO), G, Lambda
  Complex(Kind=pr), Intent(in)    :: E0, H11(NAO), H20(NAO), H02(NAO)
  Complex(Kind=pr), Intent(in)    :: H40(NAO,NAO), H04(NAO,NAO)
  Complex(Kind=pr), Intent(in)    :: H31(NAO,NAO), H13(NAO,NAO)
  Complex(Kind=pr), Intent(in)    :: H22(NAO,NAO), Hb22(NAO,NAO)
  Complex(Kind=pr), Intent(out)   :: ECC
  !Real   (Kind=pr), Intent(out)   :: Corr(nx,ny)
  Complex(Kind=pr), Intent(in) :: S00(NAO,NAO)
  Complex(Kind=pr), Intent(in) :: S20p(NAO,NAO), S20q(NAO,NAO)
  Complex(Kind=pr), Intent(in) :: S11p(NAO,NAO), S11q(NAO,NAO)
  Complex(Kind=pr), Intent(in) :: S02p(NAO,NAO), S02q(NAO,NAO)
  Complex(Kind=pr), Intent(in) :: S40(NAO,NAO), S31pq(NAO,NAO), S31qp(NAO,NAO)
  Complex(Kind=pr), Intent(in) :: S22NN(NAO,NAO)
  Complex(Kind=pr), Intent(in) :: ST22pq(NAO,NAO), ST22qp(NAO,NAO)
  Complex(Kind=pr), Intent(in) :: S13pq(NAO,NAO), S13qp(NAO,NAO)
  Complex(Kind=pr), Intent(in) :: S04(NAO,NAO)
  Real(Kind=pr), Intent(out)   :: Corr(NAO) !Change this accordingly for 1D and 2D

  Complex(Kind=pr)   :: ECC_Z

  ! Miscellaneous variables
  Real   (Kind=pr) :: dQ, Energy, Lambdatmp,x,h, tol, maxabs, relerr, denom
  Integer :: IAlloc, I, J, Iter, iG

  ! PUCC variables
  Integer :: TrunODE
  Complex (Kind=pr)              :: H00Cmplx, Ene
  Complex (Kind=pr), Allocatable :: H20Cmplx(:), H11Cmplx(:), H02Cmplx(:)
  Complex (Kind=pr), Allocatable :: H40Cmplx(:,:), H31Cmplx(:,:)
  Complex (Kind=pr), Allocatable :: H04Cmplx(:,:), H13Cmplx(:,:)
  Complex (Kind=pr), Allocatable :: H22Cmplx(:,:), HT22Cmplx(:,:)
  Complex (Kind=pr), Allocatable :: UCmplx(:), VCmplx(:)
  Complex (Kind=pr), Allocatable :: T1Cmplx(:), T2Cmplx(:,:)
  Complex (Kind=pr), Allocatable :: Z1Cmplx(:), Z2Cmplx(:,:)
  Complex (Kind=pr), Allocatable :: SpSq(:,:)                           !Vars for gradient
  Complex (kind=pr), Allocatable :: dSpSq(:,:)
  Complex (kind=pr), Allocatable :: SpSq_p(:,:), SpSq_m(:,:)
  Complex (Kind=pr), Allocatable :: SpSq_pp(:,:), SpSq_mm(:,:)
  Complex (Kind=pr)              :: NP, NP2
  Logical :: DoCCDIn

  ! Restart variables
  Logical :: ExistsA, ExistsB, Exists
 ! write(*,*) "Inside NBCSCC:"
 !write(*,*) "NAO    =", NAO
 !write(*,*) "NOcc   =", NOcc
 !write(*,*) "NPoints=", NPoints
 !write(*,*) "pBrdIn =", pBrdIn
 !write(*,*) "nx     =", nx
 !write(*,*) "ny     =", ny
 !write(*,*) "iPBC   =", iPBC
 !write(*,*) "iCorr  =", iCorr
  DoCCDIn = .FALSE.
  TrunODE = 2

  ! Initialize Corr even if skipped
  Corr = 0.0_pr

  if (nx*ny /= NAO) then
     stop "NBCSCC: nx*ny must equal NAO"
  end if

  !===============================!
  ! Prepare integrals and coeffs  !
  !===============================!
  Allocate(H20Cmplx(NAO), H11Cmplx(NAO), H02Cmplx(NAO), &
           H40Cmplx(NAO,NAO), H31Cmplx(NAO,NAO),        &
           H13Cmplx(NAO,NAO), H04Cmplx(NAO,NAO),        &
           H22Cmplx(NAO,NAO), HT22Cmplx(NAO,NAO),       &
           T1Cmplx(NAO), T2Cmplx(NAO,NAO),              &
           UCmplx(NAO), VCmplx(NAO),                    &
           Z1Cmplx(NAO), Z2Cmplx(NAO,NAO),              &
           Stat=IAlloc)
  If (IAlloc /= 0) Stop "Could not allocate in main!"

  ! Cmplexify the variables
  UCmplx   = Cmplx(U,   Kind=pr)
  VCmplx   = Cmplx(V,   Kind=pr)
  H11Cmplx = Cmplx(H11, Kind=pr)
  H20Cmplx = Cmplx(H20, Kind=pr)
  H02Cmplx = Cmplx(H02, Kind=pr)
  H22Cmplx = Cmplx(H22, Kind=pr)
  HT22Cmplx= Cmplx(Hb22,Kind=pr)
  H40Cmplx = Cmplx(H40, Kind=pr)
  H04Cmplx = Cmplx(H04, Kind=pr)
  H31Cmplx = Cmplx(H31, Kind=pr)
  H13Cmplx = Cmplx(H13, Kind=pr)
  H00Cmplx = Cmplx(E0,  Kind=pr)

  T1Cmplx = Zero
  T2Cmplx = Zero
  Z1Cmplx = Zero
  Z2Cmplx = Zero
  Lambdatmp = Lambda

  Do I = 1, NAO
    ! Zero out the diagonal blocks of H31, H13, H40 and H04
    H31Cmplx(I,I) = Zero
    H13Cmplx(I,I) = Zero
    H04Cmplx(I,I) = Zero
    H40Cmplx(I,I) = Zero
  EndDo

  !===============================!
  !         BCS CCSD              !
  !===============================!
  ExistsA = .True.
  Inquire(File="Chk_T1",Exist=Exists)
  ExistsA = Exists .and. ExistsA
  Inquire(File="Chk_T2",Exist=Exists)
  ExistsA = Exists .and. ExistsA
  If (ExistsA) then
    Open(70,File="Chk_T1")
    Read(70,*) T1Cmplx
    Close(70)
    Open(70,File="Chk_T2")
    Read(70,*) T2Cmplx
    Close(70)
  Else
    T1Cmplx = (0.0_pr, 0.0_pr)
    T2Cmplx = (0.0_pr, 0.0_pr)
  EndIf

  ExistsA = .True.
  Inquire(File="Chk_Z1",Exist=Exists)
  ExistsA = Exists .and. ExistsA
  Inquire(File="Chk_Z2",Exist=Exists)
  ExistsA = Exists .and. ExistsA
  If (ExistsA) then
    Open(70,File="Chk_Z1")
    Read(70,*) Z1Cmplx
    Close(70)
    Open(70,File="Chk_Z2")
    Read(70,*) Z2Cmplx
    Close(70)
  Else
    Z1Cmplx = (0.0_pr, 0.0_pr)
    Z2Cmplx = (0.0_pr, 0.0_pr)
  EndIf

  write(*,*)"The T1 at the beginning is ", maxval(real(T1Cmplx))
  write(*,*)"The T2 at the beginning is ", maxval(real(T2Cmplx))

  Call BroydenIterTCC(Ene,T1Cmplx,T2Cmplx,UCmplx,VCmplx,&
                      NAO,pBrdIn,DoCCDIn,               &
                      H00Cmplx,H20Cmplx,H11Cmplx,H02Cmplx, &
                      H40Cmplx,H31Cmplx,H22Cmplx,HT22Cmplx,&
                      H13Cmplx,H04Cmplx)

  Call BroydenIterTCC_Z(Z1Cmplx,Z2Cmplx,T1Cmplx,T2Cmplx,UCmplx,VCmplx,&
                        NAO,pBrdIn,DoCCDIn,                            &
                        H00Cmplx,H20Cmplx,H11Cmplx,H02Cmplx,           &
                        H40Cmplx,H31Cmplx,H22Cmplx,HT22Cmplx,          &
                        H13Cmplx,H04Cmplx)

  Call CCNum(T1Cmplx,UCmplx,VCmplx,NP,NAO)
  Call CCSD_Z_Ene(ECC_Z,Z1Cmplx,Z2Cmplx,T1Cmplx,T2Cmplx,NAO, &
                  H20Cmplx,H11Cmplx,H02Cmplx,H40Cmplx,        &
                  H31Cmplx,H22Cmplx,HT22Cmplx,H13Cmplx,H04Cmplx)

  Print *, "N(CCSD) =", real(NP,kind=pr)
  Print *, "Z equations successful"
  Print *, "Energy(Z) =", real(ECC_Z,kind=pr) + real(H00Cmplx,kind=pr)
  Print *, "H00Cmplx = ", real(H00Cmplx,kind=pr)
  Print *, "Energy(CCSD) =", Ene

  ECC = Ene

  !===============================!
  ! Optional correlation function !
  !===============================!
  h = 1.0e-3_pr  ! FOR GRADIENT STEP
  tol = 1.0e-8_pr
  If (iCorr == 1) Then

    Allocate(SpSq(NAO,NAO),SpSq_m(NAO,NAO), dSpSq(NAO,NAO),SpSq_pp(NAO,NAO),SpSq_mm(NAO,NAO), SpSq_p(NAO,NAO), Stat=IAlloc)
    If (IAlloc /= 0) Stop "Could not allocate SpSq"
    !T2Cmplx(1,2) = 0.0_pr     !DIAGONISTIC TEST
    !Z2Cmplx(1,2) = 0.0_pr

    Call CCSD_SpSq(SpSq, T1Cmplx, T2Cmplx, Z1Cmplx, Z2Cmplx, NAO, &
     S00, S20p, S20q, S11p, S11q, S02p, S02q, &
     S40, S31pq, S31qp, S22NN, ST22pq, ST22qp, S13pq, S13qp, S04)
    print*,"CCSD_SpSq Done!"
    !print *, 'SpSq(1,1)=', SpSq(1,1)
    !print *, 'SpSq(2,2)=', SpSq(2,2)
    print *, 'SpSq(3,4)=', SpSq(3,4)
    print *, 'SpSq(4,3)=', SpSq(4,3)
    print*, "shape of SpSq = ",shape(SpSq)

    !========NUMERICAL GRADIENT==========================
    !Gradient calculation, take step size h
    ! L'(x) at x = +2h
    x = 2.0_pr*h
    Call CCSD_SpSq_Grad(x, &
         T1Cmplx, T2Cmplx, &
         Z1Cmplx, Z2Cmplx, &
         NAO, &
         H00Cmplx, H20Cmplx, H11Cmplx, H02Cmplx, H40Cmplx, H31Cmplx, &
         H22Cmplx, HT22Cmplx, H13Cmplx, H04Cmplx, &
         S00, S20p, S20q, S11p, S11q, S02p, S02q, &
         S40, S31pq, S31qp, S22NN, ST22pq, ST22qp, S13pq, S13qp, S04, &
         SpSq_pp)
    
    x = h
    Call CCSD_SpSq_Grad(x, &
         T1Cmplx, T2Cmplx, &
         Z1Cmplx, Z2Cmplx, &
         NAO, &
         H00Cmplx, H20Cmplx, H11Cmplx, H02Cmplx, H40Cmplx, H31Cmplx, &
         H22Cmplx, HT22Cmplx, H13Cmplx, H04Cmplx, &
         S00, S20p, S20q, S11p, S11q, S02p, S02q, &
         S40, S31pq, S31qp, S22NN, ST22pq, ST22qp, S13pq, S13qp, S04, &
         SpSq_p)

    ! Value at x = -h
    x = -h
    Call CCSD_SpSq_Grad(x, &
         T1Cmplx, T2Cmplx, &
         Z1Cmplx, Z2Cmplx, &
         NAO, &
         H00Cmplx, H20Cmplx, H11Cmplx, H02Cmplx, H40Cmplx, H31Cmplx, &
         H22Cmplx, HT22Cmplx, H13Cmplx, H04Cmplx, &
         S00, S20p, S20q, S11p, S11q, S02p, S02q, &
         S40, S31pq, S31qp, S22NN, ST22pq, ST22qp, S13pq, S13qp, S04, &
         SpSq_m)

     ! L'(x) at x = -2h
    x = -2.0_pr*h
    Call CCSD_SpSq_Grad(x, &
         T1Cmplx, T2Cmplx, &
         Z1Cmplx, Z2Cmplx, &
         NAO, &
         H00Cmplx, H20Cmplx, H11Cmplx, H02Cmplx, H40Cmplx, H31Cmplx, &
         H22Cmplx, HT22Cmplx, H13Cmplx, H04Cmplx, &
         S00, S20p, S20q, S11p, S11q, S02p, S02q, &
         S40, S31pq, S31qp, S22NN, ST22pq, ST22qp, S13pq, S13qp, S04, &
         SpSq_mm)

    ! Central-difference derivative at x = 0
    !dSpSq = (SpSq_p - SpSq_m) / (2.0_pr*h)
    
    !5-point stencil numerical derivative at x = 0
    dSpSq = (-SpSq_pp + 8.0_pr*SpSq_p - 8.0_pr*SpSq_m + SpSq_mm) / (12.0_pr*h)

    print *, "Numerical derivative dSpSq/dx at x=0:"
    print *, 'dSpSq(3,4)=', dSpSq(3,4)
    print *, 'dSpSq(4,3)=', dSpSq(4,3)

    maxabs = (abs(dSpSq(3,4) - SpSq(3,4)))
    denom  = max((abs(SpSq(3,4))), 1.0_pr )
    relerr = maxabs / denom

    print *, "h                 =", h
    print *, "max |num-ana|     =", maxabs
    print *, "relative error    =", relerr

    if (maxabs < tol .or. relerr < tol) then
       print *, "SpSq gradient check PASSED"
    else
       print *, "SpSq gradient check FAILED"
    end if
    !=========END GRADIENT CHECK============================!

    Call CalcCorr1D_FromSpSq(SpSq, NAO, iPBC, Corr)
    !Call CalcCorr2D_FromSpSq(SpSq,nx,ny,iPBC,Corr)

    
    Deallocate(SpSq,SpSq_pp,SpSq_mm, SpSq_p, SpSq_m, dSpSq, Stat=IAlloc)
    If (IAlloc /= 0) Stop "Could not deallocate SpSq"

  End If

  Deallocate(H20Cmplx, H11Cmplx, H02Cmplx,              &
             H40Cmplx, H31Cmplx, H13Cmplx, H04Cmplx,    &
             H22Cmplx, HT22Cmplx, T1Cmplx, T2Cmplx,     &
             Z1Cmplx, Z2Cmplx, UCmplx, VCmplx,          &
             Stat=IAlloc)
  If (IAlloc /= 0) Stop "Could not deallocate in main!"

End Subroutine NBCSCC



Subroutine CalcCorr2D_FromSpSq(SpSq, nx, ny, iPBC, Corr)
  Use Precision
  Implicit None

  Integer, Intent(in) :: nx, ny, iPBC
  Complex(Kind=pr), Intent(in) :: SpSq(nx*ny, nx*ny)
  Real(Kind=pr), Intent(out) :: Corr(nx,ny)

  Integer :: dx, dy
  Integer :: x1, y1, x2, y2
  Integer :: p, q
  Integer :: count
  Complex(Kind=pr) :: sumv, termv
  Real(Kind=pr), Parameter :: tol = 1.0e-10_pr

  Corr = 0.0_pr
  write(*,*) "Entered CalcCorr2D_FromSpSq, iPBC =", iPBC, " nx  =", nx, " ny =",ny

  do dx = 0, nx-1
    do dy = 0, ny-1

      sumv  = (0.0_pr, 0.0_pr)
      count = 0

      do x1 = 1, nx
        do y1 = 1, ny

          x2 = x1 + dx
          y2 = y1 + dy

          if (iPBC == 1) then
            x2 = mod(x2-1, nx) + 1
            y2 = mod(y2-1, ny) + 1
          else
            
            if (x2 > nx .or. y2 > ny) cycle
          end if

          p = (x1-1)*ny + y1
          q = (x2-1)*ny + y2

          ! Hermitian symmetrization for numerical safety
          termv = 0.5_pr * ( SpSq(p,q) + conjg(SpSq(q,p)) )

          sumv = sumv + termv
          count = count + 1

        end do
      end do

      if (count > 0) then
        if (abs(aimag(sumv)) > tol) then
          print *, "Warning: Im(Corr) at dx,dy = ", dx, dy, &
                   " value = ", aimag(sumv)/real(count,kind=pr)
        end if
        Corr(dx+1,dy+1) = real(sumv) / real(count,kind=pr)
      else
        Corr(dx+1,dy+1) = 0.0_pr
      end if

    end do
  end do
  End Subroutine CalcCorr2D_FromSpSq


  Subroutine CalcCorr1D_FromSpSq(SpSq, NAO, iPBC, Corr)
  Use Precision
  Implicit None

  Integer, Intent(in) :: NAO, iPBC
  Complex(Kind=pr), Intent(in) :: SpSq(NAO,NAO)
  Real(Kind=pr), Intent(out) :: Corr(NAO)

  Integer :: r, p, q, count
  Complex(Kind=pr) :: sumv, termv
  Real(Kind=pr), Parameter :: tol = 1.0e-10_pr

  Corr = 0.0_pr
  write(*,*) "Entered CalcCorr1D_FromSpSq, iPBC =", iPBC, " NAO =", NAO
  do r = 0, NAO-1
    sumv  = (0.0_pr, 0.0_pr)
    count = 0

    if (iPBC == 1) then
      do p = 1, NAO
        q = mod(p-1+r, NAO) + 1
        !termv = 0.5_pr * (SpSq(p,q) + conjg(SpSq(q,p)))
        sumv = sumv + SpSq(p,q)
        count = count + 1
      end do
    else
     
      do p = 1, NAO-r
        q = p + r
        !termv = 0.5_pr * (SpSq(p,q) + conjg(SpSq(q,p)))
        sumv = sumv + SpSq(p,q)
        count = count + 1
      end do
    end if
   ! print*,"sumv = ",sumv
    if (count > 0) then
      if (abs(aimag(sumv)) > tol) then
        print *, "Warning: Im(Corr) at r = ", r, &
                 " value = ", aimag(sumv)/real(count,kind=pr)
      end if
      Corr(r+1) = real(sumv) / real(count,kind=pr)
    else
      Corr(r+1) = 0.0_pr
    end if
  end do

End Subroutine CalcCorr1D_FromSpSq


