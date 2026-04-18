
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


    complex(kind=pr), dimension(:), allocatable :: tau0_npnq

    complex(kind=pr), dimension(:, :), allocatable :: tau1_npnq

    complex(kind=pr), dimension(:), allocatable :: tau2_npnq

    complex(kind=pr), dimension(:, :), allocatable :: tau3_npnq

    complex(kind=pr), dimension(:, :), allocatable :: tau4_npnq

    complex(kind=pr), dimension(:, :), allocatable :: tau5_npnq

    complex(kind=pr), dimension(:), allocatable :: tau6_npnq

    complex(kind=pr), dimension(:), allocatable :: tau7_npnq

    complex(kind=pr), dimension(:, :), allocatable :: tau8_npnq

    complex(kind=pr), dimension(:, :), allocatable :: tau9_npnq

    complex(kind=pr), dimension(:, :), allocatable :: tau10_npnq

    complex(kind=pr), dimension(:, :), allocatable :: tau11_npnq

    complex(kind=pr), dimension(:, :), allocatable :: tau12_npnq

    complex(kind=pr), dimension(:, :), allocatable :: tau13_npnq

    complex(kind=pr), dimension(:, :), allocatable :: tau14_npnq

    complex(kind=pr), dimension(:, :), allocatable :: tau15_npnq

    complex(kind=pr), dimension(:), allocatable :: tau16_npnq

    complex(kind=pr), dimension(:), allocatable :: tau17_npnq

    complex(kind=pr), dimension(:), allocatable :: tau18_npnq

    complex(kind=pr), dimension(:), allocatable :: tau19_npnq

    complex(kind=pr), dimension(:), allocatable :: tau20_npnq

    complex(kind=pr), dimension(:), allocatable :: tau21_npnq

    complex(kind=pr), dimension(:), allocatable :: tau22_npnq

    !$omp parallel default(shared)

    allocate(tau0_npnq(NAO))
    !$omp single
    tau0_npnq = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau0_npnq(p) = tau0_npnq(p) + ( &
                t2(p, q) * z2(p, q)&
            )
        end do
    end do
    !$omp end do

    allocate(tau4_npnq(NAO, NAO))
    !$omp single
    tau4_npnq = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau4_npnq(p, q) = tau4_npnq(p, q) + ( &
                t1(q) * tau0_npnq(p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau6_npnq(NAO))
    !$omp single
    tau6_npnq = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau6_npnq(p) = tau6_npnq(p) + ( &
            tau0_npnq(p)&
        )
    
    end do
    !$omp end do

    allocate(tau8_npnq(NAO, NAO))
    !$omp single
    tau8_npnq = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau8_npnq(p, q) = tau8_npnq(p, q) + ( &
                2 * tau0_npnq(q) * t2(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau8_npnq(p, q) = tau8_npnq(p, q) + ( &
                2 * t1(p) * t1(q) * tau0_npnq(p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau15_npnq(NAO, NAO))
    !$omp single
    tau15_npnq = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau15_npnq(p, q) = tau15_npnq(p, q) + ( &
                t1(q) * tau0_npnq(p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau20_npnq(NAO))
    !$omp single
    tau20_npnq = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau20_npnq(p) = tau20_npnq(p) + ( &
            6 * tau0_npnq(p) * N04(p, p)&
        )
    
    end do
    !$omp end do

    allocate(tau21_npnq(NAO))
    !$omp single
    tau21_npnq = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau21_npnq(p) = tau21_npnq(p) - ( &
            4 * tau0_npnq(p) * N22NN(p, p)&
        )
    
    end do
    !$omp end do

    allocate(tau22_npnq(NAO))
    !$omp single
    tau22_npnq = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau22_npnq(p) = tau22_npnq(p) + ( &
            2 * t1(p) * tau0_npnq(p)&
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
                2 * t1(q) * tau0_npnq(p) * N31pq(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau1_npnq(NAO, NAO))
    !$omp single
    tau1_npnq = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau1_npnq(p, q) = tau1_npnq(p, q) + ( &
                    t2(p, r) * z2(q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    allocate(tau3_npnq(NAO, NAO))
    !$omp single
    tau3_npnq = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau3_npnq(p, q) = tau3_npnq(p, q) - ( &
                t1(p)**2 * tau1_npnq(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau8_npnq(p, q) = tau8_npnq(p, q) - ( &
                    t2(p, r) * tau1_npnq(q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    allocate(tau13_npnq(NAO, NAO))
    !$omp single
    tau13_npnq = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau13_npnq(p, q) = tau13_npnq(p, q) + ( &
                tau1_npnq(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau16_npnq(NAO))
    !$omp single
    tau16_npnq = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau16_npnq(p) = tau16_npnq(p) + ( &
                t2(p, q) * tau1_npnq(p, q)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau1_npnq)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau21_npnq(p) = tau21_npnq(p) + ( &
            tau16_npnq(p) * N04(p, p)&
        )
    
    end do
    !$omp end do

    deallocate(tau16_npnq)

    allocate(tau2_npnq(NAO))
    !$omp single
    tau2_npnq = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau2_npnq(p) = tau2_npnq(p) + ( &
                z1(q) * t2(p, q)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau3_npnq(p, q) = tau3_npnq(p, q) + ( &
                t1(p) * tau2_npnq(q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau8_npnq(p, q) = tau8_npnq(p, q) - ( &
                tau3_npnq(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau8_npnq(p, q) = tau8_npnq(p, q) - ( &
                tau3_npnq(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau3_npnq)

    allocate(tau18_npnq(NAO))
    !$omp single
    tau18_npnq = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau18_npnq(p) = tau18_npnq(p) + ( &
            tau2_npnq(p) * N04(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau22_npnq(p) = tau22_npnq(p) - ( &
            tau2_npnq(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau2_npnq)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau4_npnq(p, q) = tau4_npnq(p, q) + ( &
                z1(p) * t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau4_npnq(p, q) = tau4_npnq(p, q) - ( &
                2 * t1(q) * t2(p, q) * z2(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau8_npnq(p, q) = tau8_npnq(p, q) + ( &
                2 * t1(q) * tau4_npnq(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau4_npnq)

    allocate(tau5_npnq(NAO, NAO))
    !$omp single
    tau5_npnq = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau5_npnq(p, q) = tau5_npnq(p, q) + ( &
                t1(p) * z1(q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau5_npnq(p, q) = tau5_npnq(p, q) - ( &
                t1(p)**2 * z2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau8_npnq(p, q) = tau8_npnq(p, q) + ( &
                t1(q)**2 * tau5_npnq(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau5_npnq)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau6_npnq(p) = tau6_npnq(p) + ( &
            t1(p) * z1(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau8_npnq(p, q) = tau8_npnq(p, q) + ( &
                2 * tau6_npnq(p) * t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            NpNq(p, q) = NpNq(p, q) + ( &
                2 * tau6_npnq(p) * N11p(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            NpNq(p, q) = NpNq(p, q) + ( &
                2 * tau6_npnq(q) * N11q(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau6_npnq)

    allocate(tau7_npnq(NAO))
    !$omp single
    tau7_npnq = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau7_npnq(p) = tau7_npnq(p) + ( &
            t1(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau7_npnq(p) = tau7_npnq(p) - ( &
            t1(p)**2 * z1(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau8_npnq(p, q) = tau8_npnq(p, q) - ( &
                t1(q) * tau7_npnq(p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau7_npnq)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau8_npnq(p, q) = tau8_npnq(p, q) - ( &
                t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau8_npnq(p, q) = tau8_npnq(p, q) - ( &
                2 * t2(q, p)**2 * z2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            NpNq(p, q) = NpNq(p, q) - ( &
                N40(q, p) * tau8_npnq(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau8_npnq)

    allocate(tau9_npnq(NAO, NAO))
    !$omp single
    tau9_npnq = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau9_npnq(p, q) = tau9_npnq(p, q) + ( &
                t1(q) * N31pq(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau9_npnq(p, q) = tau9_npnq(p, q) + ( &
                t1(p) * N31qp(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau9_npnq(p, q) = tau9_npnq(p, q) - ( &
                N22NN(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau12_npnq(NAO, NAO))
    !$omp single
    tau12_npnq = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau12_npnq(p, q) = tau12_npnq(p, q) + ( &
                4 * t2(q, p) * tau9_npnq(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau9_npnq)

    allocate(tau10_npnq(NAO, NAO))
    !$omp single
    tau10_npnq = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau10_npnq(p, q) = tau10_npnq(p, q) + ( &
                N13pq(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau10_npnq(p, q) = tau10_npnq(p, q) + ( &
                2 * t1(p) * N22NN(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau10_npnq(p, q) = tau10_npnq(p, q) - ( &
                t1(p)**2 * N31qp(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau12_npnq(p, q) = tau12_npnq(p, q) - ( &
                2 * t1(p) * tau10_npnq(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau10_npnq)

    allocate(tau11_npnq(NAO, NAO))
    !$omp single
    tau11_npnq = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau11_npnq(p, q) = tau11_npnq(p, q) + ( &
                NT22pq(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau11_npnq(p, q) = tau11_npnq(p, q) + ( &
                2 * t1(p) * N31pq(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau12_npnq(p, q) = tau12_npnq(p, q) + ( &
                t1(p)**2 * tau11_npnq(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau12_npnq(p, q) = tau12_npnq(p, q) - ( &
                N04(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau12_npnq(p, q) = tau12_npnq(p, q) + ( &
                t1(q)**2 * NT22qp(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau12_npnq(p, q) = tau12_npnq(p, q) - ( &
                2 * t1(q) * N13qp(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            NpNq(p, q) = NpNq(p, q) - ( &
                tau12_npnq(q, p) * z2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau12_npnq)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau13_npnq(p, q) = tau13_npnq(p, q) + ( &
                t1(p) * z1(q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau15_npnq(p, q) = tau15_npnq(p, q) + ( &
                t1(p) * tau13_npnq(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            NpNq(p, q) = NpNq(p, q) + ( &
                2 * N31qp(p, q) * tau15_npnq(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau15_npnq)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            NpNq(p, q) = NpNq(p, q) + ( &
                tau11_npnq(p, q) * tau13_npnq(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau11_npnq)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            NpNq(p, q) = NpNq(p, q) + ( &
                NT22qp(p, q) * tau13_npnq(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau13_npnq)

    allocate(tau14_npnq(NAO, NAO))
    !$omp single
    tau14_npnq = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau14_npnq(p, q) = tau14_npnq(p, q) + ( &
                z1(p) * N31pq(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau14_npnq(p, q) = tau14_npnq(p, q) + ( &
                z1(q) * N31qp(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            NpNq(p, q) = NpNq(p, q) + ( &
                2 * t2(q, p) * tau14_npnq(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau14_npnq)

    allocate(tau17_npnq(NAO))
    !$omp single
    tau17_npnq = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau17_npnq(p) = tau17_npnq(p) + ( &
            N31pq(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau17_npnq(p) = tau17_npnq(p) + ( &
            N31qp(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau18_npnq(p) = tau18_npnq(p) + ( &
            2 * tau0_npnq(p) * tau17_npnq(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau0_npnq)

    allocate(tau19_npnq(NAO))
    !$omp single
    tau19_npnq = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau19_npnq(p) = tau19_npnq(p) + ( &
            t1(p)**2 * tau17_npnq(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau17_npnq)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau18_npnq(p) = tau18_npnq(p) - ( &
            2 * z1(p) * N22NN(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau21_npnq(p) = tau21_npnq(p) + ( &
            2 * t1(p) * tau18_npnq(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau18_npnq)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau19_npnq(p) = tau19_npnq(p) - ( &
            t1(p)**3 * N04(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau21_npnq(p) = tau21_npnq(p) + ( &
            2 * tau19_npnq(p) * z1(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau19_npnq)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau20_npnq(p) = tau20_npnq(p) - ( &
            N04(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau21_npnq(p) = tau21_npnq(p) - ( &
            t1(p)**2 * tau20_npnq(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau20_npnq)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            NpNq(p, q) = NpNq(p, q) - ( &
                deltaf(p, q) * tau21_npnq(p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau21_npnq)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau22_npnq(p) = tau22_npnq(p) - ( &
            t1(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau22_npnq(p) = tau22_npnq(p) + ( &
            t1(p)**2 * z1(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            NpNq(p, q) = NpNq(p, q) - ( &
                tau22_npnq(p) * N20p(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            NpNq(p, q) = NpNq(p, q) - ( &
                tau22_npnq(q) * N20q(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau22_npnq)

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