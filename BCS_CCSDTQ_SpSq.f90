Module CCSDTQSpSq
    Use Precision
    Use Constants

    Contains

subroutine CCSDTQ_SpSq(SpSq, T1,T2,T3,T4, z1, z2,z3,z4, NAO, &
     S00, S20p, S20q, S11p, S11q, S02p, S02q, &
     S40, S31pq, S31qp, S22NN, ST22pq, ST22qp, S13pq, S13qp, S04)

   use Precision
   implicit none
   integer, intent(in) :: NAO
   complex(kind=pr), intent(in) :: T1(NAO), T2(NAO,NAO), T3(NAO,NAO,NAO), T4(NAO,NAO,NAO,NAO)
   complex(kind=pr), intent(in) :: z1(NAO), z2(NAO,NAO), z3(NAO,NAO,NAO), z4(NAO,NAO,NAO,NAO)

   complex(kind=pr), intent(in) :: S00(NAO,NAO)
   complex(kind=pr), intent(in) :: S20p(NAO,NAO), S20q(NAO,NAO)
   complex(kind=pr), intent(in) :: S11p(NAO,NAO), S11q(NAO,NAO)
   complex(kind=pr), intent(in) :: S02p(NAO,NAO), S02q(NAO,NAO)
   complex(kind=pr), intent(in) :: S40(NAO,NAO), S04(NAO,NAO)
   complex(kind=pr), intent(in) :: S31pq(NAO,NAO), S31qp(NAO,NAO)
   complex(kind=pr), intent(in) :: S22NN(NAO,NAO)
   complex(kind=pr), intent(in) :: ST22pq(NAO,NAO), ST22qp(NAO,NAO)
   complex(kind=pr), intent(in) :: S13pq(NAO,NAO), S13qp(NAO,NAO)
   integer                      :: p,q,r,s,i,j,k,l
   complex(kind=pr), intent(out) :: SpSq(NAO,NAO)
   complex(kind=pr) :: tmp

 

    complex(kind=pr) , dimension(NAO, NAO) :: tau0
    complex(kind=pr) , dimension(NAO, NAO) :: tau1
    complex(kind=pr) , dimension(NAO, NAO) :: tau2
    complex(kind=pr) , dimension(NAO) :: tau3
    complex(kind=pr) , dimension(NAO, NAO) :: tau4
    complex(kind=pr) , dimension(NAO, NAO) :: tau5
    complex(kind=pr) , dimension(NAO) :: tau6
    complex(kind=pr) , dimension(NAO, NAO) :: tau7
    complex(kind=pr) , dimension(NAO, NAO) :: tau8
    complex(kind=pr) , dimension(NAO, NAO) :: tau9
    complex(kind=pr) , dimension(NAO, NAO) :: tau10
    complex(kind=pr) , dimension(NAO, NAO) :: tau11
    complex(kind=pr) , dimension(NAO, NAO) :: tau12
    complex(kind=pr) , dimension(NAO, NAO) :: tau13
    complex(kind=pr) , dimension(NAO) :: tau14
    complex(kind=pr) , dimension(NAO) :: tau15
    complex(kind=pr) , dimension(NAO) :: tau16
    complex(kind=pr) , dimension(NAO) :: tau17
    complex(kind=pr) , dimension(NAO) :: tau18
    complex(kind=pr) , dimension(NAO) :: tau19
     

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
        do q=1, NAO
            tau1(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau1(p, q) = tau1(p, q) + ( &
                t1(p) * z1(q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau1(p, q) = tau1(p, q) - ( &
                z2(q, p) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau1(p, q) = tau1(p, q) + ( &
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
            tau2(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau2(p, q) = tau2(p, q) + ( &
                ST22pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau2(p, q) = tau2(p, q) + ( &
                2 * t1(p) * S13pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau2(p, q) = tau2(p, q) - ( &
                S04(q, p) * t1(p)**2 &
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
                t2(q, p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau4(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau4(p, q) = tau4(p, q) + ( &
                S04(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau4(p, q) = tau4(p, q) + ( &
                2 * z1(p) * S13qp(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau4(p, q) = tau4(p, q) + ( &
                4 * S22NN(q, p) * z2(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau4(p, q) = tau4(p, q) - ( &
                2 * tau3(p) * S04(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau4(p, q) = tau4(p, q) - ( &
                2 * tau3(q) * S04(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau5(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau5(p, q) = tau5(p, q) + ( &
                t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau5(p, q) = tau5(p, q) + ( &
                t1(p) * t1(q) &
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
            tau7(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau7(p, q) = tau7(p, q) + ( &
                t2(q, p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau7(p, q) = tau7(p, q) + ( &
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
            tau8(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau8(p, q) = tau8(p, q) - ( &
                z2(q, p) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau8(p, q) = tau8(p, q) + ( &
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
            tau9(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau9(p, q) = tau9(p, q) + ( &
                2 * t1(p) * t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau9(p, q) = tau9(p, q) + ( &
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
            tau10(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau10(p, q) = tau10(p, q) - ( &
                2 * t1(q) * z1(q) * t2(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau10(p, q) = tau10(p, q) + ( &
                t1(p) * tau6(q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau10(p, q) = tau10(p, q) + ( &
                t1(q) * tau6(p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau10(p, q) = tau10(p, q) + ( &
                2 * tau7(q, p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau10(p, q) = tau10(p, q) + ( &
                    t2(r, q) * tau8(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau10(p, q) = tau10(p, q) - ( &
                z1(p) * tau9(p, q) &
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
                z2(q, p) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau11(p, q) = tau11(p, q) - ( &
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
                2 * t1(p) * t2(p, q) * z2(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau12(p, q) = tau12(p, q) - ( &
                t1(p) * tau3(q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau12(p, q) = tau12(p, q) + ( &
                t1(q) * tau11(p, q) &
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
                z1(p) * t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau13(p, q) = tau13(p, q) - ( &
                2 * t1(q) * t2(q, p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau13(p, q) = tau13(p, q) + ( &
                t1(q) * tau3(p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau14(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau14(p) = tau14(p) + ( &
            t1(p) * S13pq(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau14(p) = tau14(p) + ( &
            t1(p) * S13qp(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau14(p) = tau14(p) - ( &
            S22NN(p, p) &
        )
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
        tau15(p) = tau15(p) + ( &
            S13pq(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau15(p) = tau15(p) + ( &
            S13qp(p, p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau16(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau16(p) = tau16(p) - ( &
            2 * t1(p) * S22NN(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau16(p) = tau16(p) + ( &
            tau15(p) * t1(p)**2 &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau17(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau17(p) = tau17(p) + ( &
                2 * tau14(p) * t2(q, p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau17(p) = tau17(p) + ( &
            tau16(p) * z1(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau18(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau18(p) = tau18(p) - ( &
            t1(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau18(p) = tau18(p) + ( &
            z1(p) * t1(p)**2 &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau18(p) = tau18(p) - ( &
            tau6(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau18(p) = tau18(p) + ( &
            2 * t1(p) * tau3(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau19(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau19(p) = tau19(p) + ( &
            t1(p) * z1(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau19(p) = tau19(p) + ( &
            tau3(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                z1(p) * S20p(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                z1(q) * S20q(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                S40(p, q) * z2(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * t1(p) * S31qp(p, q) * z2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * t1(q) * S31pq(p, q) * z2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                S00(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                tau1(q, p) * tau2(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                tau4(q, p) * tau5(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                S04(q, p) * tau10(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * S13qp(p, q) * tau12(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                ST22qp(p, q) * tau1(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * S13pq(p, q) * tau13(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) - ( &
                2*deltaf(p, q) * tau17(p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) - ( &
                tau18(p) * S02p(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) - ( &
                tau18(q) * S02q(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * tau19(p) * S11p(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * tau19(q) * S11q(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel
    do p = 1, NAO
	SpSq(p,p) = 0.75_pr
    do q = p+1, NAO
       tmp = 0.5_pr * (SpSq(p,q) + (SpSq(q,p)))
       SpSq(p,q) = tmp
       SpSq(q,p) = (tmp)
    end do
    end do
End Subroutine CCSDTQ_SpSq

double precision function deltaf(p, q)
   implicit none
   integer, intent(in) :: p, q

   if (p == q) then
    deltaf = 1.0d0
   else
    deltaf = 0.0d0
   end if

   end function deltaf

End Module CCSDTQSpSq
