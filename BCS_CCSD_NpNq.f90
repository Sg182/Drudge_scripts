
Module CCSDNpNq
    Use Precision
    Use Constants

    Contains

subroutine CCSD_NpNq(NpNq, T1, T2, z1, z2, NAO, &
     N00, N20p, N20q, N11p, N11q, N02p, N02q, &
     N40, N31pq, N31qp, N22NN, NT22pq, NT22qp, N13pq, N13qp, N04)

    use Precision
    implicit none
    integer, intent(in) :: NAO
    complex(kind=pr), intent(in) :: T1(NAO), T2(NAO,NAO)
    complex(kind=pr), intent(in) :: z1(NAO), z2(NAO,NAO)

    complex(kind=pr), intent(in) :: N00(NAO,NAO)
    complex(kind=pr), intent(in) :: N20p(NAO,NAO), N20q(NAO,NAO)
    complex(kind=pr), intent(in) :: N11p(NAO,NAO), N11q(NAO,NAO)
    complex(kind=pr), intent(in) :: N02p(NAO,NAO), N02q(NAO,NAO)
    complex(kind=pr), intent(in) :: N40(NAO,NAO), N04(NAO,NAO)
    complex(kind=pr), intent(in) :: N31pq(NAO,NAO), N31qp(NAO,NAO)
    complex(kind=pr), intent(in) :: N22NN(NAO,NAO)
    complex(kind=pr), intent(in) :: NT22pq(NAO,NAO), NT22qp(NAO,NAO)
    complex(kind=pr), intent(in) :: N13pq(NAO,NAO), N13qp(NAO,NAO)
    integer                      :: p,q,r,s,i,j,k,l
    complex(kind=pr), intent(out) :: NpNq(NAO,NAO)
    complex(kind=pr) :: tmp


    complex(kind=pr), dimension(:), allocatable :: tau0

    complex(kind=pr), dimension(:, :), allocatable :: tau1

    complex(kind=pr), dimension(:), allocatable :: tau2

    complex(kind=pr), dimension(:, :), allocatable :: tau3

    complex(kind=pr), dimension(:, :), allocatable :: tau4

    complex(kind=pr), dimension(:, :), allocatable :: tau5

    complex(kind=pr), dimension(:), allocatable :: tau6

    complex(kind=pr), dimension(:), allocatable :: tau7

    complex(kind=pr), dimension(:, :), allocatable :: tau8

    complex(kind=pr), dimension(:, :), allocatable :: tau9

    complex(kind=pr), dimension(:, :), allocatable :: tau10

    complex(kind=pr), dimension(:, :), allocatable :: tau11

    complex(kind=pr), dimension(:, :), allocatable :: tau12

    complex(kind=pr), dimension(:, :), allocatable :: tau13

    complex(kind=pr), dimension(:, :), allocatable :: tau14

    complex(kind=pr), dimension(:, :), allocatable :: tau15

    complex(kind=pr), dimension(:), allocatable :: tau16

    complex(kind=pr), dimension(:), allocatable :: tau17

    complex(kind=pr), dimension(:), allocatable :: tau18

    complex(kind=pr), dimension(:), allocatable :: tau19

    complex(kind=pr), dimension(:), allocatable :: tau20

    complex(kind=pr), dimension(:), allocatable :: tau21

    complex(kind=pr), dimension(:), allocatable :: tau22

    !$omp parallel default(shared)

    allocate(tau0(NAO))
    !$omp single
    tau0 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau0(p) = tau0(p) + ( &
                t2(p, q) * z2(p, q)&
            )
        end do
    end do
    !$omp end do

    allocate(tau4(NAO, NAO))
    !$omp single
    tau4 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau4(p, q) = tau4(p, q) + ( &
                t1(q) * tau0(p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau6(NAO))
    !$omp single
    tau6 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau6(p) = tau6(p) + ( &
            tau0(p)&
        )
    
    end do
    !$omp end do

    allocate(tau8(NAO, NAO))
    !$omp single
    tau8 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau8(p, q) = tau8(p, q) + ( &
                2 * tau0(q) * t2(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau8(p, q) = tau8(p, q) + ( &
                2 * t1(p) * t1(q) * tau0(p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau15(NAO, NAO))
    !$omp single
    tau15 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau15(p, q) = tau15(p, q) + ( &
                t1(q) * tau0(p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau20(NAO))
    !$omp single
    tau20 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau20(p) = tau20(p) + ( &
            6 * tau0(p) * N04(p, p)&
        )
    
    end do
    !$omp end do

    allocate(tau21(NAO))
    !$omp single
    tau21 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau21(p) = tau21(p) - ( &
            4 * tau0(p) * N22NN(p, p)&
        )
    
    end do
    !$omp end do

    allocate(tau22(NAO))
    !$omp single
    tau22 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau22(p) = tau22(p) + ( &
            2 * t1(p) * tau0(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    NpNq = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            NpNq(p, q) = NpNq(p, q) + ( &
                2 * t1(q) * tau0(p) * N31pq(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau1(NAO, NAO))
    !$omp single
    tau1 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau1(p, q) = tau1(p, q) + ( &
                    t2(p, r) * z2(q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    allocate(tau3(NAO, NAO))
    !$omp single
    tau3 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau3(p, q) = tau3(p, q) - ( &
                t1(p)**2 * tau1(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau8(p, q) = tau8(p, q) - ( &
                    t2(p, r) * tau1(q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    allocate(tau13(NAO, NAO))
    !$omp single
    tau13 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau13(p, q) = tau13(p, q) + ( &
                tau1(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau16(NAO))
    !$omp single
    tau16 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau16(p) = tau16(p) + ( &
                t2(p, q) * tau1(p, q)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau1)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau21(p) = tau21(p) + ( &
            tau16(p) * N04(p, p)&
        )
    
    end do
    !$omp end do

    deallocate(tau16)

    allocate(tau2(NAO))
    !$omp single
    tau2 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau2(p) = tau2(p) + ( &
                z1(q) * t2(p, q)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau3(p, q) = tau3(p, q) + ( &
                t1(p) * tau2(q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau8(p, q) = tau8(p, q) - ( &
                tau3(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau8(p, q) = tau8(p, q) - ( &
                tau3(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau3)

    allocate(tau18(NAO))
    !$omp single
    tau18 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau18(p) = tau18(p) + ( &
            tau2(p) * N04(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau22(p) = tau22(p) - ( &
            tau2(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau2)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau4(p, q) = tau4(p, q) + ( &
                z1(p) * t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau4(p, q) = tau4(p, q) - ( &
                2 * t1(q) * t2(p, q) * z2(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau8(p, q) = tau8(p, q) + ( &
                2 * t1(q) * tau4(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau4)

    allocate(tau5(NAO, NAO))
    !$omp single
    tau5 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau5(p, q) = tau5(p, q) + ( &
                t1(p) * z1(q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau5(p, q) = tau5(p, q) - ( &
                t1(p)**2 * z2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau8(p, q) = tau8(p, q) + ( &
                t1(q)**2 * tau5(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau5)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau6(p) = tau6(p) + ( &
            t1(p) * z1(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau8(p, q) = tau8(p, q) + ( &
                2 * tau6(p) * t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            NpNq(p, q) = NpNq(p, q) + ( &
                2 * tau6(p) * N11p(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            NpNq(p, q) = NpNq(p, q) + ( &
                2 * tau6(q) * N11q(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau6)

    allocate(tau7(NAO))
    !$omp single
    tau7 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau7(p) = tau7(p) + ( &
            t1(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau7(p) = tau7(p) - ( &
            t1(p)**2 * z1(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau8(p, q) = tau8(p, q) - ( &
                t1(q) * tau7(p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau7)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau8(p, q) = tau8(p, q) - ( &
                t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau8(p, q) = tau8(p, q) - ( &
                2 * t2(q, p)**2 * z2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            NpNq(p, q) = NpNq(p, q) - ( &
                N40(q, p) * tau8(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau8)

    allocate(tau9(NAO, NAO))
    !$omp single
    tau9 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau9(p, q) = tau9(p, q) + ( &
                t1(q) * N31pq(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau9(p, q) = tau9(p, q) + ( &
                t1(p) * N31qp(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau9(p, q) = tau9(p, q) - ( &
                N22NN(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau12(NAO, NAO))
    !$omp single
    tau12 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau12(p, q) = tau12(p, q) + ( &
                4 * t2(q, p) * tau9(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau9)

    allocate(tau10(NAO, NAO))
    !$omp single
    tau10 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau10(p, q) = tau10(p, q) + ( &
                N13pq(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau10(p, q) = tau10(p, q) + ( &
                2 * t1(p) * N22NN(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau10(p, q) = tau10(p, q) - ( &
                t1(p)**2 * N31qp(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau12(p, q) = tau12(p, q) - ( &
                2 * t1(p) * tau10(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau10)

    allocate(tau11(NAO, NAO))
    !$omp single
    tau11 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau11(p, q) = tau11(p, q) + ( &
                NT22pq(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau11(p, q) = tau11(p, q) + ( &
                2 * t1(p) * N31pq(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau12(p, q) = tau12(p, q) + ( &
                t1(p)**2 * tau11(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau12(p, q) = tau12(p, q) - ( &
                N04(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau12(p, q) = tau12(p, q) + ( &
                t1(q)**2 * NT22qp(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau12(p, q) = tau12(p, q) - ( &
                2 * t1(q) * N13qp(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            NpNq(p, q) = NpNq(p, q) - ( &
                tau12(q, p) * z2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau12)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau13(p, q) = tau13(p, q) + ( &
                t1(p) * z1(q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau15(p, q) = tau15(p, q) + ( &
                t1(p) * tau13(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            NpNq(p, q) = NpNq(p, q) + ( &
                2 * N31qp(p, q) * tau15(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau15)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            NpNq(p, q) = NpNq(p, q) + ( &
                tau11(p, q) * tau13(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau11)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            NpNq(p, q) = NpNq(p, q) + ( &
                NT22qp(p, q) * tau13(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau13)

    allocate(tau14(NAO, NAO))
    !$omp single
    tau14 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau14(p, q) = tau14(p, q) + ( &
                z1(p) * N31pq(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau14(p, q) = tau14(p, q) + ( &
                z1(q) * N31qp(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            NpNq(p, q) = NpNq(p, q) + ( &
                2 * t2(q, p) * tau14(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau14)

    allocate(tau17(NAO))
    !$omp single
    tau17 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau17(p) = tau17(p) + ( &
            N31pq(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau17(p) = tau17(p) + ( &
            N31qp(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau18(p) = tau18(p) + ( &
            2 * tau0(p) * tau17(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau0)

    allocate(tau19(NAO))
    !$omp single
    tau19 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau19(p) = tau19(p) + ( &
            t1(p)**2 * tau17(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau17)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau18(p) = tau18(p) - ( &
            2 * z1(p) * N22NN(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau21(p) = tau21(p) + ( &
            2 * t1(p) * tau18(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau18)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau19(p) = tau19(p) - ( &
            t1(p)**3 * N04(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau21(p) = tau21(p) + ( &
            2 * tau19(p) * z1(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau19)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau20(p) = tau20(p) - ( &
            N04(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau21(p) = tau21(p) - ( &
            t1(p)**2 * tau20(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau20)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            NpNq(p, q) = NpNq(p, q) - ( &
                deltaf(p, q) * tau21(p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau21)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau22(p) = tau22(p) - ( &
            t1(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau22(p) = tau22(p) + ( &
            t1(p)**2 * z1(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            NpNq(p, q) = NpNq(p, q) - ( &
                tau22(p) * N20p(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            NpNq(p, q) = NpNq(p, q) - ( &
                tau22(q) * N20q(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau22)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            NpNq(p, q) = NpNq(p, q) + ( &
                z1(p) * N02p(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            NpNq(p, q) = NpNq(p, q) + ( &
                z1(q) * N02q(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            NpNq(p, q) = NpNq(p, q) + ( &
                N00(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp end parallel

    End Subroutine CCSD_NpNq

pure double precision function deltaf(p, q)
  implicit none
  integer, intent(in) :: p, q

  if (p == q) then
    deltaf = 1.0d0
  else
    deltaf = 0.0d0
  end if

end function deltaf
End Module CCSDNpNq