Module CCSDSpSqGrad
    Use Precision
    Use Constants

    Contains

subroutine CCSD_SpSq_Grad(x, &
    T1, T2, &
    Z1, Z2, &
    NAO, &
    H00, H20, H11, H02, H40, H31, H22, HT22, H13, H04, &
    S00, S20p, S20q, S11p, S11q, S02p, S02q, &
    S40, S31pq, S31qp, S22NN, ST22pq, ST22qp, S13pq, S13qp, S04, &
    dSpSq)

   use Precision
   implicit none
   integer, intent(in) :: NAO
   complex(kind=pr), intent(in) :: T1(NAO), T2(NAO,NAO)
   complex(kind=pr), intent(in) :: z1(NAO), z2(NAO,NAO)
   complex(kind=pr) , intent(in)    :: x
   complex(kind=pr), intent(in) :: S00(NAO,NAO)
   complex(kind=pr), intent(in) :: S20p(NAO,NAO), S20q(NAO,NAO)
   complex(kind=pr), intent(in) :: S11p(NAO,NAO), S11q(NAO,NAO)
   complex(kind=pr), intent(in) :: S02p(NAO,NAO), S02q(NAO,NAO)
   complex(kind=pr), intent(in) :: S40(NAO,NAO), S04(NAO,NAO)
   complex(kind=pr), intent(in) :: S31pq(NAO,NAO), S31qp(NAO,NAO)
   complex(kind=pr), intent(in) :: S22NN(NAO,NAO)
   complex(kind=pr), intent(in) :: ST22pq(NAO,NAO), ST22qp(NAO,NAO)
   complex(kind=pr), intent(in) :: S13pq(NAO,NAO), S13qp(NAO,NAO)
   Complex (Kind=pr), Intent(In)    :: H00, H20(NAO), H11(NAO), H02(NAO)
   Complex (Kind=pr), Intent(In)    :: H40(NAO,NAO), H31(NAO,NAO)
   Complex (Kind=pr), Intent(In)    :: H04(NAO,NAO), H13(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: H22(NAO,NAO), HT22(NAO,NAO)
   integer                      :: p,q,r,s,i,j,k,l
   complex(kind=pr), intent(out) :: dSpSq(NAO,NAO)
   complex(kind=pr) :: tmp

    complex(kind=pr) , dimension(NAO, NAO) :: tau0
    complex(kind=pr) , dimension(NAO) :: tau1
    complex(kind=pr) , dimension(NAO) :: tau2
    complex(kind=pr) , dimension(NAO) :: tau3
    complex(kind=pr) , dimension(NAO) :: tau4
    complex(kind=pr) , dimension(NAO) :: tau5
    complex(kind=pr) , dimension(NAO) :: tau6
    complex(kind=pr) , dimension(NAO) :: tau7
    complex(kind=pr) , dimension(NAO) :: tau8
    complex(kind=pr) , dimension(NAO) :: tau9
    complex(kind=pr) , dimension(NAO) :: tau10
    complex(kind=pr) , dimension(NAO, NAO) :: tau11
    complex(kind=pr) , dimension(NAO, NAO) :: tau12
    complex(kind=pr) , dimension(NAO, NAO) :: tau13
    complex(kind=pr) , dimension(NAO, NAO) :: tau14
    complex(kind=pr) , dimension(NAO) :: tau15
    complex(kind=pr) , dimension(NAO, NAO) :: tau16
    complex(kind=pr) , dimension(NAO, NAO) :: tau17
    complex(kind=pr) , dimension(NAO, NAO) :: tau18
    complex(kind=pr) , dimension(NAO, NAO) :: tau19
    complex(kind=pr) , dimension(NAO, NAO) :: tau20
    complex(kind=pr) , dimension(NAO, NAO) :: tau21
    complex(kind=pr) , dimension(NAO) :: tau22
    complex(kind=pr) , dimension(NAO) :: tau23
    complex(kind=pr) , dimension(NAO) :: tau24
    complex(kind=pr) , dimension(NAO) :: tau25
    complex(kind=pr) , dimension(NAO) :: tau26
    complex(kind=pr) , dimension(NAO) :: tau27
    complex(kind=pr) , dimension(NAO, NAO) :: tau28
    complex(kind=pr) , dimension(NAO, NAO) :: tau29
    complex(kind=pr) , dimension(NAO, NAO) :: tau30
    complex(kind=pr) , dimension(NAO) :: tau31
    complex(kind=pr) , dimension(NAO) :: tau32
    complex(kind=pr) , dimension(NAO) :: tau33
    complex(kind=pr) , dimension(NAO) :: tau34
    complex(kind=pr) , dimension(NAO, NAO) :: tau35
    complex(kind=pr) , dimension(NAO) :: tau36
     

    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau0(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau0(p, q) = tau0(p, q) + ( &
                    t2(r, p) * z2(r, q) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau1(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau1(p) = tau1(p) + ( &
                t2(q, p) * tau0(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau2(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau2(p) = tau2(p) + ( &
                t2(q, p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau3(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau3(p) = tau3(p) + ( &
                H40(q, p) * t2(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau4(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau4(p) = tau4(p) + ( &
                H31(p, q) * tau0(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau5(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau5(p) = tau5(p) + ( &
                t1(q) * H31(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau6(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau6(p) = tau6(p) + ( &
                t1(q) * H40(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau7(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau7(p) = tau7(p) + ( &
                H13(q, p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau8(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau8(p) = tau8(p) + ( &
                H31(q, p) * t2(p, q) * z2(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau9(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau9(p) = tau9(p) + ( &
                t1(q) * H22(p, q) * z2(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau10(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau10(p) = tau10(p) + ( &
                t1(q) * H40(p, q) * t2(q, p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau11(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau11(p, q) = tau11(p, q) + ( &
                t1(p) * z1(q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau11(p, q) = tau11(p, q) - ( &
                z2(q, p) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau11(p, q) = tau11(p, q) + ( &
                tau0(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau12(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau12(p, q) = tau12(p, q) + ( &
                ST22pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau12(p, q) = tau12(p, q) + ( &
                2 * t1(p) * S13pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau12(p, q) = tau12(p, q) - ( &
                S04(q, p) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau13(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau13(p, q) = tau13(p, q) + ( &
                S04(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau13(p, q) = tau13(p, q) + ( &
                2 * z1(p) * S13qp(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau13(p, q) = tau13(p, q) + ( &
                4 * S22NN(q, p) * z2(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau13(p, q) = tau13(p, q) - ( &
                2 * tau2(p) * S04(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau13(p, q) = tau13(p, q) - ( &
                2 * tau2(q) * S04(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau14(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau14(p, q) = tau14(p, q) + ( &
                t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau14(p, q) = tau14(p, q) + ( &
                t1(p) * t1(q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau15(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau15(p) = tau15(p) + ( &
                z1(q) * t2(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau16(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau16(p, q) = tau16(p, q) + ( &
                t2(q, p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau16(p, q) = tau16(p, q) + ( &
                2 * t1(p) * t1(q) * t2(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau17(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau17(p, q) = tau17(p, q) - ( &
                z2(q, p) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau17(p, q) = tau17(p, q) + ( &
                tau0(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau18(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau18(p, q) = tau18(p, q) + ( &
                2 * t1(p) * t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau18(p, q) = tau18(p, q) + ( &
                t1(q) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau19(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau19(p, q) = tau19(p, q) - ( &
                2 * t1(q) * z1(q) * t2(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau19(p, q) = tau19(p, q) + ( &
                t1(p) * tau15(q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau19(p, q) = tau19(p, q) + ( &
                t1(q) * tau15(p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau19(p, q) = tau19(p, q) + ( &
                2 * tau16(q, p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau19(p, q) = tau19(p, q) + ( &
                    t2(r, q) * tau17(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau19(p, q) = tau19(p, q) - ( &
                z1(p) * tau18(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau20(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau20(p, q) = tau20(p, q) - ( &
                t1(p) * tau2(q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau20(p, q) = tau20(p, q) - ( &
                t1(q) * tau0(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau20(p, q) = tau20(p, q) + ( &
                tau18(p, q) * z2(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau21(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau21(p, q) = tau21(p, q) + ( &
                z1(p) * t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau21(p, q) = tau21(p, q) - ( &
                2 * t1(q) * t2(q, p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau21(p, q) = tau21(p, q) + ( &
                t1(q) * tau2(p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau22(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau22(p) = tau22(p) + ( &
            t1(p) * S13pq(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau22(p) = tau22(p) + ( &
            t1(p) * S13qp(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau22(p) = tau22(p) - ( &
            S22NN(p, p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau23(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau23(p) = tau23(p) + ( &
            S13pq(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau23(p) = tau23(p) + ( &
            S13qp(p, p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau24(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau24(p) = tau24(p) - ( &
            2 * t1(p) * S22NN(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau24(p) = tau24(p) + ( &
            tau23(p) * t1(p)**2 &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau25(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau25(p) = tau25(p) + ( &
                2 * tau22(p) * t2(q, p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau25(p) = tau25(p) + ( &
            tau24(p) * z1(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau26(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau26(p) = tau26(p) - ( &
            t1(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau26(p) = tau26(p) + ( &
            z1(p) * t1(p)**2 &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau26(p) = tau26(p) - ( &
            tau15(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau26(p) = tau26(p) + ( &
            2 * t1(p) * tau2(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau27(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau27(p) = tau27(p) + ( &
            t1(p) * z1(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau27(p) = tau27(p) + ( &
            tau2(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau28(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau28(p, q) = tau28(p, q) + ( &
                    H40(r, p) * t2(r, q) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau29(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau29(p, q) = tau29(p, q) + ( &
                HT22(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau29(p, q) = tau29(p, q) + ( &
                tau28(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau30(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau30(p, q) = tau30(p, q) + ( &
                H04(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau30(p, q) = tau30(p, q) + ( &
                4 * H22(q, p) * t2(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau30(p, q) = tau30(p, q) + ( &
                2 * H40(p, q) * t2(p, q)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau30(p, q) = tau30(p, q) + ( &
                    t2(r, p) * tau29(r, q) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau31(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau31(p) = tau31(p) + ( &
            H20(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau31(p) = tau31(p) - ( &
            2 * t1(p) * H40(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau31(p) = tau31(p) + ( &
            2 * tau6(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau32(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau32(p) = tau32(p) + ( &
            H11(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau32(p) = tau32(p) + ( &
            2 * H22(p, p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau33(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau33(p) = tau33(p) + ( &
            H20(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau33(p) = tau33(p) + ( &
            2 * H31(p, p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau34(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau34(p) = tau34(p) + ( &
            H02(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau34(p) = tau34(p) + ( &
            2 * H40(p, p) * t1(p)**3 &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau34(p) = tau34(p) + ( &
                t1(q) * HT22(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau34(p) = tau34(p) + ( &
                2 * H31(p, q) * tau14(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau34(p) = tau34(p) - ( &
                2 * H40(q, p) * tau18(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau34(p) = tau34(p) + ( &
                tau31(q) * t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau34(p) = tau34(p) + ( &
            2 * t1(p) * tau32(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau34(p) = tau34(p) - ( &
            tau33(p) * t1(p)**2 &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau35(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau35(p, q) = tau35(p, q) + ( &
                HT22(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau35(p, q) = tau35(p, q) + ( &
                2 * t1(p) * H31(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau35(p, q) = tau35(p, q) - ( &
                H40(q, p) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau35(p, q) = tau35(p, q) - ( &
                6 * H40(q, q) * t2(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau35(p, q) = tau35(p, q) + ( &
                2 * tau28(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau36(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau36(p) = tau36(p) + ( &
            H40(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau36(p) = tau36(p) + ( &
                tau35(q, p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = dSpSq(p, q) + ( &
                H00 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = dSpSq(p, q) + ( &
                x * S00(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = dSpSq(p, q) + ( &
                x * z1(p) * S20p(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = dSpSq(p, q) + ( &
                x * z1(q) * S20q(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = dSpSq(p, q) + ( &
                x * S40(p, q) * z2(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = dSpSq(p, q) + ( &
                2*x * t1(p) * S31qp(p, q) * z2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = dSpSq(p, q) + ( &
                2*x * t1(q) * S31pq(p, q) * z2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    H20(r) * t1(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    tau1(r) * H40(r, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    4 * tau2(r) * tau3(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    2 * t1(r) * tau4(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    2 * tau2(r) * tau5(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    4 * t1(r) * tau2(r) * tau6(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    dSpSq(p, q) = dSpSq(p, q) + ( &
                        H40(s, r) * t2(s, r) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    t1(r) * tau6(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    2 * H11(r) * tau2(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    2 * t1(r) * tau7(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    4 * tau2(r) * H22(r, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    4 * t1(r) * tau2(r) * H31(r, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    4 * t1(r) * tau8(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    2 * H20(r) * t1(r) * tau2(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    4 * t1(r) * tau9(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    4 * t1(r) * tau10(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = dSpSq(p, q) + ( &
                x * tau11(q, p) * tau12(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = dSpSq(p, q) + ( &
                x * tau13(q, p) * tau14(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = dSpSq(p, q) + ( &
                x * S04(q, p) * tau19(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = dSpSq(p, q) - ( &
                2*x * S13qp(p, q) * tau20(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = dSpSq(p, q) + ( &
                x * ST22qp(p, q) * tau11(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = dSpSq(p, q) + ( &
                2*x * S13pq(p, q) * tau21(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = dSpSq(p, q) - ( &
                2*x*deltaf(p, q) * tau25(p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = dSpSq(p, q) - ( &
                x * tau26(p) * S02p(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = dSpSq(p, q) - ( &
                x * tau26(q) * S02q(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = dSpSq(p, q) + ( &
                2*x * tau27(p) * S11p(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = dSpSq(p, q) + ( &
                2*x * tau27(q) * S11q(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    dSpSq(p, q) = dSpSq(p, q) + ( &
                        tau30(p, q) * z2(p, q) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    tau34(p) * z1(p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    tau36(p) * t1(p)**2 &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel
    do p = 1, NAO
	do q = p+1, NAO
       tmp = 0.5_pr * (dSpSq(p,q) + (dSpSq(q,p)))
       dSpSq(p,q) = tmp
       dSpSq(q,p) = (tmp)
    end do
    end do
End Subroutine CCSD_SpSq_Grad

pure double precision function deltaf(p, q)
   implicit none
   integer, intent(in) :: p, q

   if (p == q) then
    deltaf = 1.0d0
   else
    deltaf = 0.0d0
   end if

   end function deltaf

End Module CCSDSpSqGrad