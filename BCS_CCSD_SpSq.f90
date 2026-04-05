Module CCSDSpSq
    Use Precision
    Use Constants

    Contains

subroutine CCSD_SpSq(SpSq, T1, T2, z1, z2, NAO, &
     S00, S20p, S20q, S11p, S11q, S02p, S02q, &
     S40, S31pq, S31qp, S22NN, ST22pq, ST22qp, S13pq, S13qp, S04)

   use Precision
   implicit none
   integer, intent(in) :: NAO
   complex(kind=pr), intent(in) :: T1(NAO), T2(NAO,NAO)
   complex(kind=pr), intent(in) :: z1(NAO), z2(NAO,NAO)

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



    complex(kind=pr) , dimension(:), allocatable :: spsq_tau0

    complex(kind=pr) , dimension(:, :), allocatable :: spsq_tau1

    complex(kind=pr) , dimension(:), allocatable :: spsq_tau2

    complex(kind=pr) , dimension(:, :), allocatable :: spsq_tau3

    complex(kind=pr) , dimension(:, :), allocatable :: spsq_tau4

    complex(kind=pr) , dimension(:), allocatable :: spsq_tau5

    complex(kind=pr) , dimension(:), allocatable :: spsq_tau6

    complex(kind=pr) , dimension(:, :), allocatable :: spsq_tau7

    complex(kind=pr) , dimension(:, :), allocatable :: spsq_tau8

    complex(kind=pr) , dimension(:, :), allocatable :: spsq_tau9

    complex(kind=pr) , dimension(:, :), allocatable :: spsq_tau10

    complex(kind=pr) , dimension(:, :), allocatable :: spsq_tau11

    complex(kind=pr) , dimension(:, :), allocatable :: spsq_tau12

    complex(kind=pr) , dimension(:, :), allocatable :: spsq_tau13

    complex(kind=pr) , dimension(:, :), allocatable :: spsq_tau14

    complex(kind=pr) , dimension(:), allocatable :: spsq_tau15

    complex(kind=pr) , dimension(:), allocatable :: spsq_tau16

    complex(kind=pr) , dimension(:), allocatable :: spsq_tau17

    complex(kind=pr) , dimension(:), allocatable :: spsq_tau18


    allocate(spsq_tau0(NAO))
    spsq_tau0 = 0.0

    do p=1, NAO
        do q=1, NAO
            spsq_tau0(p) = spsq_tau0(p) + ( &
                t2(p, q) * z2(p, q)&
            )
        end do
    end do

    allocate(spsq_tau3(NAO, NAO))
    spsq_tau3 = 0.0

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau3(p, q) = spsq_tau3(p, q) + ( &
                2 * t1(p) * t1(q) * spsq_tau0(p)&
            )
    
        end do
    end do

    allocate(spsq_tau5(NAO))
    spsq_tau5 = 0.0

    do p=1, NAO
    
        spsq_tau5(p) = spsq_tau5(p) + ( &
            spsq_tau0(p)&
        )
    
    end do

    allocate(spsq_tau14(NAO, NAO))
    spsq_tau14 = 0.0

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau14(p, q) = spsq_tau14(p, q) + ( &
                t1(q) * spsq_tau0(p)&
            )
    
        end do
    end do

    allocate(spsq_tau16(NAO))
    spsq_tau16 = 0.0

    do p=1, NAO
    
        spsq_tau16(p) = spsq_tau16(p) + ( &
            2 * t1(p) * spsq_tau0(p)&
        )
    
    end do

    allocate(spsq_tau18(NAO))
    spsq_tau18 = 0.0

    do p=1, NAO
    
        spsq_tau18(p) = spsq_tau18(p) + ( &
            2 * t1(p) * spsq_tau0(p)&
        )
    
    end do

    SpSq = 0.0

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * t1(q) * spsq_tau0(p) * S31pq(p, q)&
            )
    
        end do
    end do

    deallocate(spsq_tau0)

    allocate(spsq_tau1(NAO, NAO))
    spsq_tau1 = 0.0

    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                spsq_tau1(p, q) = spsq_tau1(p, q) + ( &
                    t2(p, r) * z2(q, r)&
                )
            end do
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau3(p, q) = spsq_tau3(p, q) + ( &
                t1(p)**2 * spsq_tau1(q, p)&
            )
    
        end do
    end do

    allocate(spsq_tau7(NAO, NAO))
    spsq_tau7 = 0.0

    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                spsq_tau7(p, q) = spsq_tau7(p, q) - ( &
                    t2(q, r) * spsq_tau1(p, r)&
                )
            end do
        end do
    end do

    allocate(spsq_tau12(NAO, NAO))
    spsq_tau12 = 0.0

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau12(p, q) = spsq_tau12(p, q) + ( &
                spsq_tau1(p, q)&
            )
    
        end do
    end do

    deallocate(spsq_tau1)

    allocate(spsq_tau2(NAO))
    spsq_tau2 = 0.0

    do p=1, NAO
        do q=1, NAO
            spsq_tau2(p) = spsq_tau2(p) + ( &
                z1(q) * t2(p, q)&
            )
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau3(p, q) = spsq_tau3(p, q) - ( &
                t1(p) * spsq_tau2(q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau7(p, q) = spsq_tau7(p, q) + ( &
                spsq_tau3(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau7(p, q) = spsq_tau7(p, q) + ( &
                spsq_tau3(q, p)&
            )
    
        end do
    end do

    deallocate(spsq_tau3)

    do p=1, NAO
    
        spsq_tau18(p) = spsq_tau18(p) - ( &
            spsq_tau2(p)&
        )
    
    end do

    deallocate(spsq_tau2)

    allocate(spsq_tau4(NAO, NAO))
    spsq_tau4 = 0.0

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau4(p, q) = spsq_tau4(p, q) + ( &
                2 * t2(q, p)**2&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau4(p, q) = spsq_tau4(p, q) + ( &
                t1(p)**2 * t1(q)**2&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau4(p, q) = spsq_tau4(p, q) + ( &
                4 * t1(p) * t1(q) * t2(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau7(p, q) = spsq_tau7(p, q) - ( &
                spsq_tau4(q, p) * z2(q, p)&
            )
    
        end do
    end do

    deallocate(spsq_tau4)

    do p=1, NAO
    
        spsq_tau5(p) = spsq_tau5(p) + ( &
            t1(p) * z1(p)&
        )
    
    end do

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau7(p, q) = spsq_tau7(p, q) + ( &
                2 * spsq_tau5(q) * t2(q, p)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau7(p, q) = spsq_tau7(p, q) + ( &
                2 * spsq_tau5(p) * t2(q, p)&
            )
    
        end do
    end do

    allocate(spsq_tau17(NAO))
    spsq_tau17 = 0.0

    do p=1, NAO
    
        spsq_tau17(p) = spsq_tau17(p) - ( &
            2 * spsq_tau5(p) * S22NN(p, p)&
        )
    
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * spsq_tau5(p) * S11p(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * spsq_tau5(q) * S11q(p, q)&
            )
    
        end do
    end do

    deallocate(spsq_tau5)

    allocate(spsq_tau6(NAO))
    spsq_tau6 = 0.0

    do p=1, NAO
    
        spsq_tau6(p) = spsq_tau6(p) + ( &
            t1(p)&
        )
    
    end do

    do p=1, NAO
    
        spsq_tau6(p) = spsq_tau6(p) - ( &
            t1(p)**2 * z1(p)&
        )
    
    end do

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau7(p, q) = spsq_tau7(p, q) - ( &
                t1(q) * spsq_tau6(p)&
            )
    
        end do
    end do

    deallocate(spsq_tau6)

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau7(p, q) = spsq_tau7(p, q) - ( &
                t2(q, p)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau7(p, q) = spsq_tau7(p, q) + ( &
                t1(q)**2 * t1(p) * z1(q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                S40(q, p) * spsq_tau7(q, p)&
            )
    
        end do
    end do

    deallocate(spsq_tau7)

    allocate(spsq_tau8(NAO, NAO))
    spsq_tau8 = 0.0

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau8(p, q) = spsq_tau8(p, q) + ( &
                t1(q) * S31pq(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau8(p, q) = spsq_tau8(p, q) + ( &
                t1(p) * S31qp(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau8(p, q) = spsq_tau8(p, q) - ( &
                S22NN(p, q)&
            )
    
        end do
    end do

    allocate(spsq_tau11(NAO, NAO))
    spsq_tau11 = 0.0

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau11(p, q) = spsq_tau11(p, q) + ( &
                4 * t2(q, p) * spsq_tau8(q, p)&
            )
    
        end do
    end do

    deallocate(spsq_tau8)

    allocate(spsq_tau9(NAO, NAO))
    spsq_tau9 = 0.0

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau9(p, q) = spsq_tau9(p, q) + ( &
                S13pq(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau9(p, q) = spsq_tau9(p, q) + ( &
                2 * t1(p) * S22NN(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau9(p, q) = spsq_tau9(p, q) - ( &
                t1(p)**2 * S31qp(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau11(p, q) = spsq_tau11(p, q) - ( &
                2 * t1(p) * spsq_tau9(q, p)&
            )
    
        end do
    end do

    deallocate(spsq_tau9)

    allocate(spsq_tau10(NAO, NAO))
    spsq_tau10 = 0.0

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau10(p, q) = spsq_tau10(p, q) + ( &
                ST22pq(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau10(p, q) = spsq_tau10(p, q) + ( &
                2 * t1(p) * S31pq(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau11(p, q) = spsq_tau11(p, q) + ( &
                t1(p)**2 * spsq_tau10(q, p)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau11(p, q) = spsq_tau11(p, q) - ( &
                S04(q, p)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau11(p, q) = spsq_tau11(p, q) + ( &
                t1(q)**2 * ST22qp(q, p)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau11(p, q) = spsq_tau11(p, q) - ( &
                2 * t1(q) * S13qp(q, p)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                spsq_tau11(q, p) * z2(q, p)&
            )
    
        end do
    end do

    deallocate(spsq_tau11)

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau12(p, q) = spsq_tau12(p, q) + ( &
                t1(p) * z1(q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau14(p, q) = spsq_tau14(p, q) + ( &
                t1(p) * spsq_tau12(q, p)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * S31qp(p, q) * spsq_tau14(q, p)&
            )
    
        end do
    end do

    deallocate(spsq_tau14)

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                spsq_tau10(p, q) * spsq_tau12(q, p)&
            )
    
        end do
    end do

    deallocate(spsq_tau10)

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                ST22qp(p, q) * spsq_tau12(p, q)&
            )
    
        end do
    end do

    deallocate(spsq_tau12)

    allocate(spsq_tau13(NAO, NAO))
    spsq_tau13 = 0.0

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau13(p, q) = spsq_tau13(p, q) + ( &
                z1(p) * S31pq(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            spsq_tau13(p, q) = spsq_tau13(p, q) + ( &
                z1(q) * S31qp(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * t2(q, p) * spsq_tau13(p, q)&
            )
    
        end do
    end do

    deallocate(spsq_tau13)

    allocate(spsq_tau15(NAO))
    spsq_tau15 = 0.0

    do p=1, NAO
    
        spsq_tau15(p) = spsq_tau15(p) + ( &
            S31pq(p, p)&
        )
    
    end do

    do p=1, NAO
    
        spsq_tau15(p) = spsq_tau15(p) + ( &
            S31qp(p, p)&
        )
    
    end do

    do p=1, NAO
    
        spsq_tau16(p) = spsq_tau16(p) + ( &
            t1(p)**2 * z1(p)&
        )
    
    end do

    do p=1, NAO
    
        spsq_tau17(p) = spsq_tau17(p) + ( &
            spsq_tau15(p) * spsq_tau16(p)&
        )
    
    end do

    deallocate(spsq_tau16)

    deallocate(spsq_tau15)

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2*deltaf(p, q) * spsq_tau17(p)&
            )
    
        end do
    end do

    deallocate(spsq_tau17)

    do p=1, NAO
    
        spsq_tau18(p) = spsq_tau18(p) - ( &
            t1(p)&
        )
    
    end do

    do p=1, NAO
    
        spsq_tau18(p) = spsq_tau18(p) + ( &
            t1(p)**2 * z1(p)&
        )
    
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                spsq_tau18(p) * S20p(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                spsq_tau18(q) * S20q(p, q)&
            )
    
        end do
    end do

    deallocate(spsq_tau18)

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                z1(p) * S02p(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                z1(q) * S02q(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                S00(p, q)&
            )
    
        end do
    end do

do p = 1, NAO
	SpSq(p,p) = 0.75_pr
    !do q = p+1, NAO
    !   tmp = 0.5_pr * (SpSq(p,q) + (SpSq(q,p)))
    !   SpSq(p,q) = tmp
    !   SpSq(q,p) = (tmp)
    !end do
    end do
End Subroutine CCSD_SpSq

pure double precision function deltaf(p, q)
   implicit none
   integer, intent(in) :: p, q

   if (p == q) then
    deltaf = 1.0d0
   else
    deltaf = 0.0d0
   end if

   end function deltaf

End Module CCSDSpSq